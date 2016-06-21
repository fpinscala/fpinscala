import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Defaults.coreDefaultSettings ++ Seq(
    scalaVersion := "2.11.8",
    resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts ++ Seq(
              onLoadMessage ~= (_ + nio2check())
            )) aggregate (chapterCode, exercises, answers)
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

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {Class.forName(cls); ""}
    catch {case _: ClassNotFoundException => s"""
      |WARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.
      |You are probably running Java < 1.7; answers will not compile.
      |You seem to be running ${System.getProperty("java.version")}.
      |Try `project exercises' before compile, or upgrading your JDK.""".stripMargin
    }
  }
}
