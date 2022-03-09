import mill._, scalalib._, scalafmt._

object slox extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.1.1"

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-effect:3.3.7"
  )
}
