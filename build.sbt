ThisBuild / scalaVersion := "3.1.0"
ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector", "-source:future")

lazy val root = project
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = project
lazy val answers = project
