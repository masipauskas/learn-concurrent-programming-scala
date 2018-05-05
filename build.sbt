import Dependencies._
import sbt.Keys.libraryDependencies

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "uk.co.martynas",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "learn_concurrent_programming_in_scala",
    libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.7",
    libraryDependencies ++= testDependencies
  )
