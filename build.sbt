name := "lisa"

version := "1.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.0"


enablePlugins(PackPlugin)
