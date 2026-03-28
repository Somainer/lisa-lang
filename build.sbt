name := "lisa"

version := "3.0.0"

scalaVersion := "3.3.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

// https://mvnrepository.com/artifact/org.jline/jline-terminal
libraryDependencies += "org.jline" % "jline-terminal" % "3.25.1"
// https://mvnrepository.com/artifact/org.jline/jline-reader
libraryDependencies += "org.jline" % "jline-reader" % "3.25.1"
// https://mvnrepository.com/artifact/org.jline/jline-terminal-jna
libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.25.1"

// Scala 3 staging for JIT compilation
libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:dynamics",
  "-language:implicitConversions",
)

packMain := Map("lisa" -> "moe.roselia.lisa.Main")

enablePlugins(PackPlugin)
