import mill._
import mill.define.{Sources, Target}
import scalalib._
import scalafmt._
import $ivy.`org.antlr:antlr4:4.8`
import org.antlr.v4.Tool

object parser extends JavaModule {
//    override def unmanagedClasspath: Target[Loose.Agg[PathRef]] = Agg(
//      mill.modules.Util.download(
//        "https://www.antlr.org/download/antlr-4.9.2-complete.jar",
//        RelPath("antlr.jar")
//      )
//    )
  override def ivyDeps = Agg(ivy"org.antlr:antlr4-runtime:4.8")

  def genAntlr = T {

    if (os.exists(millSourcePath / "src")) {
      os.remove.all(millSourcePath / "src")
    }

    Tool.main(Array(
      s"${millSourcePath.toString}/Expr.g4",
      "-o", s"${millSourcePath.toString}/src/litmus/antlr",
      "-package", "litmus.antlr",
      "-visitor"
    ))
  }
}

object pipemimic extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.5"

  override def moduleDeps: Seq[JavaModule] = Seq(parser)

  object test extends Tests {
    override def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.2")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
    def testOne(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}
