name := "lisa"

version := "1.2"

scalaVersion := "2.13.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.0"

packMain := Map("lisa" -> "moe.roselia.lisa.Main")

enablePlugins(PackPlugin)
