ThisBuild / scalaVersion := "3.1.0"
ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

lazy val root = project
  .aggregate(exercises, answers, tests)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = project
lazy val answers = project
lazy val tests = project
  .dependsOn(exercises)
