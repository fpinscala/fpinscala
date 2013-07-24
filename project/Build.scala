import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq("-feature", "-language:higherKinds",
      "-language:postfixOps", "-language:implicitConversions")
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts) aggregate (chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)
}

