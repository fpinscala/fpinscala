name := "fpinscala"

ThisBuild / scalaVersion := "3.3.0-RC1-bin-20221111-cc4c279-NIGHTLY"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
