ThisBuild / scalaVersion := "2.13.11"

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(
    name := "answers"
  )
