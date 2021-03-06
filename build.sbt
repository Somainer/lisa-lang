name := "lisa"

version := "2.7.2"

scalaVersion := "2.13.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.0"
libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.13.0"

// https://mvnrepository.com/artifact/org.jline/jline-terminal
libraryDependencies += "org.jline" % "jline-terminal" % "3.17.1"
// https://mvnrepository.com/artifact/org.jline/jline-reader
libraryDependencies += "org.jline" % "jline-reader" % "3.17.1"
// https://mvnrepository.com/artifact/org.jline/jline-terminal-jna
libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.17.1"


libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 13 =>
      Some("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
    case _ => None
  }
}

packMain := Map("lisa" -> "moe.roselia.lisa.Main")

enablePlugins(PackPlugin)
