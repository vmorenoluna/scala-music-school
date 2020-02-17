name := "scala-music-school"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.17.0-M1",
  "org.specs2" %% "specs2-core" % "4.8.3" % "test",
  "org.specs2" %% "specs2-scalacheck" % "4.8.3" % Test

)
