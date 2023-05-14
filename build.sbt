import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "hackerrank-tasks",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaCheck % Test,
    libraryDependencies += scalaTestPlus % Test,
    libraryDependencies += zioCore,
    libraryDependencies += zioStream,
    libraryDependencies += catsCore,
    libraryDependencies += scalacheck,
    libraryDependencies += scalatest,
//    libraryDependencies += scalatestplus
  )

enablePlugins(JmhPlugin)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
