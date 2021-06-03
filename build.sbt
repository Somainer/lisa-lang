name := "lisa"

ThisBuild / organization := "moe.lisa"

version := "3.0.0"

scalaVersion := "3.0.0"

libraryDependencies += "org.scala-lang" % "scala3-compiler_3" % "3.0.0"
libraryDependencies += "org.scala-lang.modules" % "scala-parser-combinators_3" % "2.0.0"
libraryDependencies += "org.typelevel" % "cats-core_3" % "2.6.1"

// https://mvnrepository.com/artifact/org.jline/jline-terminal
libraryDependencies += "org.jline" % "jline-terminal" % "3.17.1"
// https://mvnrepository.com/artifact/org.jline/jline-reader
libraryDependencies += "org.jline" % "jline-reader" % "3.17.1"
// https://mvnrepository.com/artifact/org.jline/jline-terminal-jna
libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.17.1"


libraryDependencies += "org.scalactic" % "scalactic_3" % "3.2.9" % Test
libraryDependencies += "org.scalatest" % "scalatest_3" % "3.2.9" % Test

libraryDependencies += "com.somainer" % "scala3-nameof_3" % "0.0.1"

packMain := Map("lisa" -> "moe.lisa.Main")

enablePlugins(PackPlugin)
