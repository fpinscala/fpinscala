ThisBuild / scalaVersion := "3.1.0"
ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

lazy val root = project
  .aggregate(exercises, answers)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = project
  .settings(
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.15.4"
  )
lazy val answers = project
