lazy val commonSettings = Seq(
  name                            := "fpinscala",
  scalaVersion                    := "2.11.11",
  resolvers                       += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  parallelExecution in Test       := false,
  connectInput                    := true,
  updateOptions                   := updateOptions.value.withCachedResolution(true),
  scalacOptions                   := commonScalacOptions,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value filterNot (_ == "-Xfatal-warnings"))

///////////////////////////////////////////////////////////
// Project definitions

lazy val root = Project("fpinscala", file("." + "fpinscala")).in(file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings: _*)
  .settings(warnUnusedImport: _*)
  .settings(inlineWarnings: _*)
  .settings(nio2checkSettings: _*)

lazy val exercises = project.in(file("exercises"))
  .settings(commonSettings: _*)
  .settings(warnUnusedImport: _*)
  .settings(inlineWarnings: _*)

lazy val answers = project.in(file("answers"))
  .settings(commonSettings: _*)
  .settings(warnUnusedImport: _*)
  .settings(inlineWarnings: _*)

///////////////////////////////////////////////////////////
// Scala compiler settings

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-dead-code",
  "-Xfuture"
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n >= 11 => Seq("-Ywarn-unused-import")
      case _                       => Seq()
    }
  },
   scalacOptions in (Compile, console) := (scalacOptions in (Compile, console)).value filterNot (_ == "-Ywarn-unused-import"),
   scalacOptions in (Test, console)    := (scalacOptions in (Compile, console)).value)

lazy val inlineWarnings = Seq(
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq()
      case _             => Seq("-Yinline-warnings")
    }
  })

///////////////////////////////////////////////////////////
// Miscellaneous settings

lazy val nio2checkSettings = Seq(
  onLoadMessage ~= (_ + nio2check())
)  

def nio2check(): String = {
  val cls = "java.nio.channels.AsynchronousFileChannel"
  try {Class.forName(cls); ""}
  catch {case _: ClassNotFoundException =>
    ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
     "You are probably running Java < 1.7; answers will not compile.\n" +
     "You seem to be running " + System.getProperty("java.version") + ".\n" +
     "Try `project exercises' before compile, or upgrading your JDK.")
  }
}

///////////////////////////////////////////////////////////
// Command aliases

addCommandAlias("validate", ";scalastyle;test")
