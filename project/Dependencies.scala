import sbt._

object Dependencies {
 lazy val testDependencies = Seq(
     "org.specs2" %% "specs2-core" % "4.0.2",
     "org.specs2" %% "specs2-matcher-extra" % "4.0.2",
     "org.specs2" %% "specs2-mock" % "4.0.2"
 ).map(_ % "test")
}
