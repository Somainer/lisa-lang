package moe.roselia.lisa.Import

import java.nio.file.{FileSystems, Path}

import moe.roselia.lisa.Environments.{EmptyEnv, Environment, NameSpacedEnv, TransparentLayer}
import moe.roselia.lisa.LispExp.{Expression, LisaMapRecord, NilObj, PrimitiveFunction, PrimitiveMacro, SString, Symbol, WrappedScalaObject}
import moe.roselia.lisa.Reflect.ScalaBridge
import moe.roselia.lisa.{LispExp, Main}
import moe.roselia.lisa.Util.Extractors.RichOption

import scala.util.DynamicVariable

class PackageImporter {
  private val packageCache = collection.mutable.Map.empty[String, Environment]
  private val currentPath = new DynamicVariable(FileSystems.getDefault.getPath("."))
  private val preludeEnv = new DynamicVariable[Environment](EmptyEnv)

  def environmentWithMeta(path: Path, environment: Environment): Environment = {
    import moe.roselia.lisa.LispExp._
    import Implicits._
    environment.withValues(Seq(
      "**PATH**" -> path.toAbsolutePath.getParent.toString,
      "**PATH-OBJ**" -> WrappedScalaObject(path),
      "**FILE**" -> path.getFileName.toString,
    ))
  }
  def environmentWithMeta(path: String, environment: Environment): Environment = {
    environmentWithMeta(FileSystems.getDefault.getPath(path).normalize(), environment)
  }
  private def environmentWithMeta(path: Path): Environment = {
    environmentWithMeta(path, preludeEnv.value)
  }
  private def resolvePath(path: String) = currentPath.value.resolve(path).normalize()
  private def executePath(file: Path) = packageCache.getOrElseUpdate(file.toAbsolutePath.toString, {
    currentPath.withValue(file.getParent) {
      val (executed, errors) = Main.executeFileImpl(file.toString, environmentWithMeta(file))
      if (errors.isDefined) {
        throw errors.get
      }
      executed.collectValues(executed.collectDefinedValues.filterNot(_.startsWith("**")).toSeq)
    }
  })
  private def selectSubEnvironment(environment: Environment, targets: Seq[String], inPath: => String): Environment = {
    val (mutableVars, immutableVars) = targets.partition(environment.isMutable)
    val mutableParts = mutableVars.foldLeft(EmptyEnv.newMutableFrame) {
      (env, key) => env.addValue(key, environment.get(key)) // If a value is mutable, it must exists.
    }
    val immutableParts = immutableVars.foldLeft(EmptyEnv.newFrame) {
      (env, key) => env.withValue(key, environment.getValueOption(key).getOrThrow(VariableNotFound(key, inPath)))
    }
    if (mutableVars.isEmpty) immutableParts
    else if (immutableVars.isEmpty) mutableParts
    else TransparentLayer(mutableParts, immutableParts)
  }

  private def doImport(pathName: String, imports: Seq[String]) = {
    val inPath = resolvePath(pathName)
    val fileName = inPath.getFileName.toString
    if (fileName.endsWith(".scala")) { // Import scala file
      ScalaImporter.compileAndLoad(inPath)
      EmptyEnv
    } else { // Import lisa file
      val sourceFile = if (fileName endsWith ".lisa") inPath else inPath.resolveSibling(s"$fileName.lisa")
      val environment = executePath(sourceFile.toAbsolutePath)
      if (imports.isEmpty) environment // Which means import all values.
      else selectSubEnvironment(environment, imports, sourceFile.toString)
    }
  }

  private def importAndMerge(pathName: String, imports: Seq[Expression], env: Environment, prefix: Option[String]) = {
    util.Try(doImport(pathName, imports.collect {
      case Symbol(str) => str
      case SString(value) => value
      case x => throw new IllegalArgumentException(s"Illegal import element: $x")
    })).recoverWith {
      case _: java.io.FileNotFoundException | _: NullPointerException => util.Failure(LisaFileNotFound(pathName))
    }.map(env => prefix.map(NameSpacedEnv(_, env)).getOrElse(env)).map(envToMerge => {
      if (envToMerge.isEmpty) env
      else TransparentLayer(envToMerge, env)
    }).map(NilObj.->).get
  }

  def setCurrentPath(path: Path): Unit = currentPath.value = path
  def setCurrentPath(path: String): Unit = setCurrentPath(FileSystems.getDefault.getPath(path))
  def withCurrentPath[T](path: String)(thunk: => T): T = currentPath.withValue(FileSystems.getDefault.getPath(path))(thunk)
  def withCurrentPath[T](path: Path)(thunk: => T): T = currentPath.withValue(path)(thunk)
  @`inline` def withCurrentFilePath[T](path: Path)(thunk: => T): T = withCurrentPath(path.getParent)(thunk)
  @`inline` def withCurrentFilePath[T](path: String)(thunk: => T): T = withCurrentFilePath(FileSystems.getDefault.getPath(path))(thunk)
  def injectRuntime(runtimeEnvironment: Environment): Unit = preludeEnv.value = runtimeEnvironment

  val importMacro: PrimitiveMacro = PrimitiveMacro {
    case (Symbol(path) :: Symbol("*") :: Symbol("as") :: Symbol(alias) :: Nil, env) =>
      importAndMerge(path, Nil, env, Some(alias))
    case (SString(path) :: Symbol("*") :: Symbol("as") :: Symbol(alias) :: Nil, env) =>
      importAndMerge(path, Nil, env, Some(alias))
    case (SString(path) :: imports, env) =>
      importAndMerge(path, imports, env, None)
    case (Symbol(path) :: imports, env) =>
      importAndMerge(path, imports, env, None)
  }.withHintProvider { input =>
    val pathTree = input.split("/", -1)
    if (pathTree.isEmpty) Nil
    else {
      val inCompleted = pathTree.last
      val completed = pathTree.init
      val resolvedPath = resolvePath(completed.mkString("/")).toAbsolutePath
      val resolvedFile = resolvedPath.toFile
      def hintOf(file: java.io.File): (String, String) = {
        val fileName =
          if (file.getName.endsWith(".lisa")) file.getName.substring(0, file.getName.length - 5) else file.getName
        val hintName = completed.appended(fileName).mkString("/")
        if (file.isDirectory) (s"$hintName/", fileName)
        else (hintName, fileName)
      }
      if (resolvedFile.exists() && resolvedFile.isDirectory) {
        resolvedFile.listFiles((_, name) => name.startsWith(inCompleted)).map(hintOf)
      } else Nil
    }
  }
  val importFunction: LispExp.SideEffectFunction = importMacro.asProcedure
  val requireFunction: PrimitiveFunction = PrimitiveFunction.withArityChecked(1) {
    case SString(path) :: Nil if path.endsWith(".scala") =>
      ScalaBridge.fromScalaNative(ScalaImporter.compileAndRequire(resolvePath(path)))
    case SString(path) :: Nil =>
      EnvironmentWrapper.wrapEnvironment(doImport(path, Nil), resolvePath(path).getFileName.toString)
  }
}

object PackageImporter extends PackageImporter
