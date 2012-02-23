import java.io.File
import sbt._
import Keys._
import collection.mutable.ListBuffer
import scala.io.Source
import nomo._

object FPinScalaBuild extends Build {

  val generate = TaskKey[Unit]("generate", "generate exercises, answers, hints, and examples")
  val cleanGenerated = TaskKey[Unit]("clean-generated", "deletes files created by generate")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "manning",
    name         := "fpinscala-exercises",
    version      := "default",
    scalaVersion := "2.9.1",
    libraryDependencies += "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9",
    generate := Master.run(
      "project/master/fpinscala.book", 
      "src/main/scala/fpinscala", 
      "src/main/resources"),
    cleanGenerated := {
      IO.delete(file("src/main/fpinscala/exercises"))
      IO.delete(file("src/main/fpinscala/answers"))
      IO.delete(file("src/main/fpinscala/examples"))
      IO.delete(file("src/main/resources/includes"))
      IO.delete(file("src/main/resources/hints"))
    }
  )

  lazy val project = Project (
    "exercises",
    file ("."),
    settings  = buildSettings 
  )
}
