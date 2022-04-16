import mill._, scalalib._, scalafmt._

object slox extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.1.2"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-effect:3.3.11",
    ivy"org.typelevel::log4cats-slf4j:2.2.0",
    ivy"ch.qos.logback:logback-classic:1.2.11",
  )

  def scalacOptions = Seq(
    "-Xfatal-warnings",
    "-Yexplicit-nulls",
  )

  object test extends Tests with ScalafmtModule {
    def ivyDeps = Agg(
      ivy"com.disneystreaming::weaver-cats:0.7.11",
      ivy"com.disneystreaming::weaver-scalacheck:0.7.11",
    )
    def testFramework = "weaver.framework.CatsEffect"
  }
}
