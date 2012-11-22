import sbt._
import Keys._

object FPInScalaBuild extends Build {
  lazy val root =
    Project(id = "fpinscala", base = file(".")) aggregate (chapterCode, exercises, answers)

  lazy val chapterCode =
    Project(id = "chapter-code", base = file("chaptercode"))

  lazy val exercises =
    Project(id = "exercises",
      base = file("exercises")) settings {
        libraryDependencies ++= Seq(
          "org.specs2" %% "specs2" % "1.11" % "test")
      }

  lazy val answers =
    Project(id = "answers", base = file("answers"))

}

