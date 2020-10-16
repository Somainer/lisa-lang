package moe.roselia.lisa.Import

import java.nio.file.Path

import scala.io.Source
import scala.tools.nsc.interpreter.IMain
import scala.tools.reflect.ToolBox
import scala.util.Using

class ScalaImporter {
  import scala.tools.nsc._
  val settings = new Settings()
  settings.classpath.value = System.getProperty("java.class.path")
  private val global = new Global(settings)
  private val run = new global.Run
  def compileAndLoad(path: Path): Unit = {
    compileAndLoad(List(path))
  }
  def compileAndLoad(paths: List[Path]): Unit = {
    run.compile(paths.map(_.toAbsolutePath.toString))
  }

  private lazy val box = scala.reflect.runtime.currentMirror.mkToolBox()
  def compileAndRequireScalaCode(source: String): Any = {
    val tree = box.parse(source)
    val compiledCode = box.compile(tree)
    compiledCode()
  }
  def compileAndRequire(path: Path): Any = {
    val source = Using(Source.fromFile(path.toFile))(_.mkString).get
    compileAndRequireScalaCode(source)
  }
}

object ScalaImporter extends ScalaImporter
