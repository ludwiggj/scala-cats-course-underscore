name := "cats_underscore"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.typelevel" %% "cats-effect" % "2.5.0",
  "org.scalatest" %% "scalatest" % "3.2.12" % "test",
  "org.typelevel" %% "cats-testkit-scalatest" % "2.1.5" % "test",
  "io.circe"      %% "circe-core" % "0.14.2",
  "io.circe"      %% "circe-parser" % "0.14.2",
  "io.circe"      %% "circe-generic" % "0.14.2"
)

// scalac options come from the sbt-tpolecat plugin so need to set any here
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
