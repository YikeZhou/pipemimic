package pipemimic

import litmus.antlr._
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

class LitmusTest

object LitmusTest {
  final private val procCnt = 2

  def apply(filePath: String): LitmusTest = {
    val reader = Source.fromFile(filePath)
    val lines = reader.getLines()
    var testName = ""
    val raw = new StringBuilder()

    for ((line, index) <- lines.zipWithIndex) {
      if (index == 0) {
        testName = line.drop(6).filterNot(c => c == '\r' || c == '\n')
        println(testName)
      } else if (index >= 9) {
        raw.append(line).append('\n')
      }
    }

    reader.close()
    println(raw.toString())
    LitmusTest(testName, parse(raw.toString()).asInstanceOf[TestCase])
  }

  def apply(name: String, testCase: TestCase): LitmusTest = {
    println(testCase)

    /* assign values to x and y if given in 'exists' */
    val memMap = mutable.Map.empty[String, Int]
    for (exp <- testCase.exp.other) {
      assert(exp.proc == -1)
      memMap += exp.loc -> exp.value.toInt
    }
    /* if not, assign an unused value to x or y */
    if (memMap.size < procCnt) {
      if (memMap.contains("x")) {
        memMap += "y" -> 1
      }
      else if (memMap.contains("y")) {
        memMap += "x" -> 1
      }
      else {
        memMap += "x" -> 1
        memMap += "y" -> 2
      }
    }

    val maxPoi = Seq(testCase.inst.core1.length, testCase.inst.core2.length)
    var eiid = 0

    val initValues = Seq(testCase.init.core1, testCase.init.core2)
    val instructions = Seq(testCase.inst.core1, testCase.inst.core2)
    val loadValues = Seq.fill(procCnt)(mutable.Map.empty[String, Int])
    Seq(testCase.exp.core0, testCase.exp.core1).zipWithIndex map { case (expList, index) =>
      expList map { exp =>
        loadValues(index) += exp.loc -> exp.value.toInt
      }
    }

    val regFile = Seq.fill(procCnt)(mutable.Map.empty[String, Int])

    /* init register file by testCase.init */
    for (proc <- 0 until procCnt) {
      for (initValue <- initValues(proc)) {
        val value = Try(initValue.value.toInt).toOption
        value match {
          case Some(num) => regFile(proc) += initValue.loc -> num
          case None => regFile(proc) += initValue.loc -> memMap(initValue.value)
        }
      }
    }

    val events = ListBuffer.empty[Event]

    for (proc <- 0 until procCnt) {
      for (poi <- 0 until maxPoi(proc)) {
        val curInst = instructions(proc)(poi)
        curInst match {
          case StoreWord(reg, addr) =>
            val access = Access(Direction.W, regFile(proc)(addr), regFile(proc)(reg))
            events += Event(eiid, Iiid(proc, poi), access)
          case LoadWord(reg, addr) =>
            val valueLoaded = loadValues(proc)(reg)
            val access = Access(Direction.R, regFile(proc)(addr), valueLoaded)
            events += Event(eiid, Iiid(proc, poi), access)
            regFile(proc)(reg) = valueLoaded
          case Fence() =>
            events += Event(eiid, Iiid(proc, poi), MemoryFence())
          case Xor(src1, src2, dst) =>
            regFile(proc)(dst) = regFile(proc)(src1) ^ regFile(proc)(src2)
          case Ori(src1, src2, dst) =>
            regFile(proc)(dst) = regFile(proc)(src1) | src2
          case Add(src1, src2, dst) =>
            regFile(proc)(dst) = regFile(proc)(src1) + regFile(proc)(src2)
          case _ =>
        }
        eiid += 1
      }
    }
    println(events)
    new LitmusTest
  }

  sealed trait Body

  case class TestCase(init: InitValues, inst: Instructions, exp: ExpectedResult) extends Body {
    override def toString: String =
      s"""Initial Values at core 1:
         |${init.core1.map(_.toString).mkString("\n")}
         |Initial Values at core 2:
         |${init.core2.map(_.toString).mkString("\n")}
         |
         |Instructions at core 1:
         |${inst.core1.map(_.toString).mkString("\n")}
         |Instructions at core 2:
         |${inst.core2.map(_.toString).mkString("\n")}
         |
         |Expected output:
         |${(exp.core0 ::: exp.core1 ::: exp.other).map(_.toString).mkString("\n")}
         |""".stripMargin
  }

  case class InitValues(core1: List[Expression], core2: List[Expression]) extends Body
  case class Expression(proc: Int, loc: String, value: String) extends Body
  case class Equation(left: String, right: String) extends Body
  case class Location(loc: String) extends Body
  case class ValueAtLocation(value: String) extends Body

  case class Instructions(core1: List[Instruction], core2: List[Instruction]) extends Body

  sealed trait Instruction extends Body
  case class Label(num: Int) extends Instruction
  case class StoreWord(reg: String, addr: String) extends Instruction
  case class LoadWord(reg: String, addr: String) extends Instruction
  case class Fence() extends Instruction
  case class BranchNotEqual(src1: String, src2: String, target: Label) extends Instruction
  case class Xor(src1: String, src2: String, dst: String) extends Instruction
  case class Ori(src1: String, src2: Int, dst: String) extends Instruction
  case class Add(src1: String, src2: String, dst: String) extends Instruction
  case class Nop() extends Instruction

  case class ExpectedResult(core0: List[Expression], core1: List[Expression], other: List[Expression]) extends Body


  def parse(input: String): Body = {
    val charStream = CharStreams.fromString(input)
    val lexer = new LitmusTestLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new LitmusTestParser(tokens)

    val visitor = new LitmusTestVisitorApp
    visitor.visit(parser.body())
  }

  class LitmusTestVisitorApp extends LitmusTestParserBaseVisitor[Body] {
    override def visitBody(ctx: LitmusTestParser.BodyContext): TestCase = {
      TestCase(visitInit(ctx.init()), visitProg(ctx.prog()), visitResult(ctx.result()))
    }

    override def visitInit(ctx: LitmusTestParser.InitContext): InitValues = visitValues(ctx.values())

    override def visitProg(ctx: LitmusTestParser.ProgContext): Instructions = visitInsts(ctx.insts())

    override def visitResult(ctx: LitmusTestParser.ResultContext): ExpectedResult = visitExps(ctx.exps())

    override def visitValues(ctx: LitmusTestParser.ValuesContext): InitValues = {
      val buf = Seq.fill(2)(ListBuffer.empty[Expression])
      for (i <- 0 until ctx.exp().size()) {
        val expression = visitExp(ctx.exp(i))
        buf(expression.proc) += expression
      }
      InitValues(buf.head.toList, buf.last.toList)
    }

    override def visitExp(ctx: LitmusTestParser.ExpContext): Expression = {
      val eq = visitEq(ctx.eq())
      if (ctx.COREID() == null)
        Expression(-1, eq.left, eq.right)
      else
        Expression(ctx.COREID().getText.stripSuffix(":").toInt, eq.left, eq.right)
    }

    override def visitEq(ctx: LitmusTestParser.EqContext): Equation = {
      val loc = visitLoc(ctx.loc())
      val value = visitValue(ctx.value())
      Equation(loc.loc, value.value)
    }

    override def visitLoc(ctx: LitmusTestParser.LocContext): Location = Location(ctx.getText)

    override def visitValue(ctx: LitmusTestParser.ValueContext): ValueAtLocation = ValueAtLocation(ctx.getText)

    override def visitInsts(ctx: LitmusTestParser.InstsContext): Instructions = {
      val buf = Seq.fill(2)(ListBuffer.empty[Instruction])
      for (i <- 0 until ctx.inst().size() by 2) {
        val instAtCore0 = visitInst(ctx.inst(i))
        val instAtCore1 = visitInst(ctx.inst(i + 1))

        if (!instAtCore0.isInstanceOf[Nop])
          buf.head += instAtCore0
        if (!instAtCore1.isInstanceOf[Nop])
          buf.last += instAtCore1
      }
      Instructions(buf.head.toList, buf.last.toList)
    }

    override def visitInst(ctx: LitmusTestParser.InstContext): Instruction = {
      if (ctx.label() != null)
        visitLabel(ctx.label())
      else if (ctx.sw() != null)
        visitSw(ctx.sw())
      else if (ctx.lw() != null)
        visitLw(ctx.lw())
      else if (ctx.fence() != null)
        visitFence(ctx.fence())
      else if (ctx.bne() != null)
        visitBne(ctx.bne())
      else if (ctx.xor() != null)
        visitXor(ctx.xor())
      else if (ctx.ori() != null)
        visitOri(ctx.ori())
      else if (ctx.add != null)
        visitAdd(ctx.add())
      else
        Nop()
    }

    override def visitLabel(ctx: LitmusTestParser.LabelContext): Label = {
      val fullLabel = ctx.LABEL().getText
      assert(fullLabel == "LC00" || fullLabel == "LC01")
      Label(fullLabel(3).toInt)
    }

    override def visitSw(ctx: LitmusTestParser.SwContext): StoreWord = {
      val reg = ctx.REG().getText
      val addr = ctx.addr().REG().getText
      StoreWord(reg, addr)
    }

    override def visitLw(ctx: LitmusTestParser.LwContext): LoadWord = {
      val reg = ctx.REG().getText
      val addr = ctx.addr().REG().getText
      LoadWord(reg, addr)
    }

    override def visitFence(ctx: LitmusTestParser.FenceContext): Fence = Fence()

    override def visitBne(ctx: LitmusTestParser.BneContext): BranchNotEqual = {
      val target = visitLabel(ctx.label())
      val src1 = ctx.REG(0).getText
      val src2 = ctx.REG(1).getText
      BranchNotEqual(src1, src2, target)
    }

    override def visitXor(ctx: LitmusTestParser.XorContext): Xor = {
      Xor(ctx.REG(1).getText, ctx.REG(2).getText, ctx.REG(0).getText)
    }

    override def visitAdd(ctx: LitmusTestParser.AddContext): Add = {
      Add(ctx.REG(1).getText, ctx.REG(2).getText, ctx.REG(0).getText)
    }

    override def visitOri(ctx: LitmusTestParser.OriContext): Ori = {
      Ori(ctx.REG(1).getText, ctx.NUMBER().getText.toInt, ctx.REG(0).getText)
    }

    override def visitExps(ctx: LitmusTestParser.ExpsContext): ExpectedResult = {
      val core0Buf = ListBuffer.empty[Expression]
      val core1Buf = ListBuffer.empty[Expression]
      val otherBuf = ListBuffer.empty[Expression]
      assert(ctx.exp().size() == 2)
      for (i <- 0 until ctx.exp().size()) {
        val expression = visitExp(ctx.exp(i))
        if (expression.proc == 0)
          core0Buf += expression
        else if (expression.proc == 1)
          core1Buf += expression
        else
          otherBuf += expression
      }
      ExpectedResult(core0Buf.toList, core1Buf.toList, otherBuf.toList)
    }
  }
}

object LitmusFileTest extends App {
  for (file <- args)
    LitmusTest(filePath = file)
}
