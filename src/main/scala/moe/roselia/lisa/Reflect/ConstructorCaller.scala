package moe.roselia.lisa.Reflect

import moe.roselia.lisa.{Environments, Evaluator, LispExp}
import moe.roselia.lisa.LispExp.PrimitiveMacro
import moe.roselia.lisa.Util.Extractors.RichOption
import moe.roselia.lisa.Reflect.ScalaBridge.{fromScalaNative, toScalaNative}

object ConstructorCaller {

  import reflect.runtime.{universe => ru}

  val searchInPackages = Seq(
    "scala",
    "scala.collection",
    "java.lang",
    "java.util",
    "moe.roselia.lisa"
  )

  def getConstructorSymbol(tpe: ru.ClassSymbol) = {
    getConstructorOfType(tpe.toType)
  }

  def resolveClassBySimpleName(name: String): Class[_] = {
    try {
      Class.forName(name)
    } catch {
      case ex: ClassNotFoundException =>
        searchInPackages.view
          .map(pkg => scala.util.Try(Class.forName(s"$pkg.$name")))
          .collectFirst {
            case scala.util.Success(clazz) => clazz
          }.getOrThrow(new ClassNotFoundException(name))
    }
  }

  def newInstanceFromClassName(name: String, args: Seq[Any]): Any = {
    val clazz = resolveClassBySimpleName(name)
    newInstanceForClass(clazz, args)
  }

  def newInstanceForClass(clazz: Class[_], args: Seq[Any]): Any = {
    val classSymbol = getSymbolForClass(clazz)
    val constructors = getConstructorOfType(classSymbol.toType)
    val constructorSymbol = findMatchingMethod(constructors, args)
      .getOrThrow(ScalaReflectionException(s"No matching constructor for ${clazz.getName} with types: ${getTypeNames(args).mkString("(", ", ", ")")}"))
    val mirror = ru.rootMirror.reflectClass(classSymbol)
    val method = mirror.reflectConstructor(constructorSymbol)
    method.apply(args: _*)
  }

  @`inline` def getTypeNames(xs: Seq[Any]): Seq[String] = xs.map(_.getClass.getName)

  @`inline` def getSymbolForClass(clazz: Class[_]): ru.ClassSymbol = {
    val runtimeMirror = ru.rootMirror
    val classSymbol = runtimeMirror.classSymbol(clazz)
    classSymbol
  }

  @`inline` def getConstructorOfType(tpe: ru.Type): ru.TermSymbol = tpe.decl(ru.termNames.CONSTRUCTOR).asTerm

  def getMirrorByName(name: String) = {
    ru.runtimeMirror(Class.forName(name).getClassLoader)
  }

  def findMatchingMethod(method: ru.TermSymbol, args: Seq[Any]): Option[ru.MethodSymbol] = {
    val overloads = method.alternatives
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(_.paramLists.flatten.length == args.length) match {
      case xs@_ :: Nil => xs
      case xs => xs.filter(sig => DotAccessor.checkTypeFits(sig.paramLists.flatten)(args))
    }
    overloads match {
      case x :: _ => Some(x)
      case Nil => None
    }
  }

  val ConstructorEnvironment: Environments.Env = Environments.EmptyEnv.withValues(Seq(
    "new" -> PrimitiveMacro {
      case (LispExp.Symbol(sym) :: args, env) =>
        Evaluator.evalList(args, env) match {
          case Right(exps) =>
            fromScalaNative(newInstanceFromClassName(sym, exps.map(toScalaNative))) -> env
          case Left(reason) => throw new RuntimeException(reason)
        }
    }.withDocString("General Constructor")
  ))
}
