import mill._, scalalib._, scalafmt._

object pipemimic extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.5"

  object test extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.2")
    def testFrameworks = Seq("org.scalatest.tools.Framework")
    def testOne(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}
