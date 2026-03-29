package moe.roselia.lisa.Import

import java.nio.file.Path
import scala.io.Source
import scala.util.Using

/**
 * ScalaImporter for Scala 3.
 * The original Scala 2 ToolBox-based approach is not available in Scala 3.
 * This provides a simplified interface using Java class loading.
 */
class ScalaImporter:
  def compileAndLoad(path: Path): Unit =
    compileAndLoad(List(path))

  def compileAndLoad(paths: List[Path]): Unit =
    // In Scala 3, direct compilation via nsc is not available.
    // Users should pre-compile Scala files separately.
    throw new UnsupportedOperationException(
      "Direct Scala file compilation is not supported in Scala 3. " +
        "Please pre-compile Scala files and use import! for Lisa files instead.")

  def compileAndRequireScalaCode(source: String): Any =
    throw new UnsupportedOperationException(
      "Runtime Scala code compilation is not supported in Scala 3 via this path. " +
        "Use Lisa's JIT capabilities or the staging API instead.")

  def compileAndRequire(path: Path): Any =
    val source = Using(Source.fromFile(path.toFile))(_.mkString).get
    compileAndRequireScalaCode(source)

object ScalaImporter extends ScalaImporter
