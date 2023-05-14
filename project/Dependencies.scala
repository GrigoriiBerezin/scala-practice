import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.15"
  lazy val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.17.0"
  lazy val zioCore = "dev.zio" %% "zio" % "2.0.10"
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.9.0"
  lazy val zioStream = "dev.zio" %% "zio-streams" % "2.0.13"

  lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"
  lazy val scalatest = "org.scalatest" %% "scalatest" % "3.2.15" % "test"
//  lazy val scalatestplus = "org.scalatestplus" %% "scalacheck" % "1.0.9" % "test"
}
