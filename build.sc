import mill._, scalalib._, scalafmt._

object slox extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.1.1"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-effect:3.3.7"
  )

  def scalacOptions = Seq(
    "-Xfatal-warnings"
  )

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats:0.7.11",
      ivy"com.disneystreaming::weaver-scalacheck:0.7.11"
    )
    def testFramework = "weaver.framework.CatsEffect"
  }
}
