package moe.roselia.lisa.Reflect

import moe.roselia.lisa.{Environments, Evaluator, LispExp}
import moe.roselia.lisa.LispExp.{PrimitiveMacro, Procedure}
import moe.roselia.lisa.Util.Extractors.RichOption
import moe.roselia.lisa.Reflect.ScalaBridge.{fromScalaNative, toScalaNative}

import java.lang.reflect.{Constructor, Modifier}

object ConstructorCaller:

  val searchInPackages = Seq(
    "scala",
    "scala.collection",
    "java.lang",
    "java.util",
    "moe.roselia.lisa"
  )

  def resolveClassBySimpleName(name: String): Class[?] =
    try
      Class.forName(name)
    catch
      case ex: ClassNotFoundException =>
        searchInPackages.view
          .map(pkg => scala.util.Try(Class.forName(s"$pkg.$name")))
          .collectFirst {
            case scala.util.Success(clazz) => clazz
          }.getOrThrow(new ClassNotFoundException(name))

  def newInstanceFromClassName(name: String, args: Seq[Any]): Any =
    val clazz = resolveClassBySimpleName(name)
    newInstanceForClass(clazz, args)

  def newInstanceForClass(clazz: Class[?], args: Seq[Any]): Any = DotAccessor.handleReflectionException {
    if args.length == 1 && args.head.isInstanceOf[Procedure] && FunctionalInterfaceAdapter.isFunctional(clazz) then
      return FunctionalInterfaceAdapter.createFunctionInvoker(clazz, args.head.asInstanceOf[Procedure])

    val constructors = clazz.getConstructors
    val matching = findMatchingConstructor(constructors.toSeq, args)
      .getOrThrow(ScalaReflectionException(
        s"No matching constructor for ${clazz.getName} with types: ${getTypeNames(args).mkString("(", ", ", ")")}"))

    val paramTypes = matching.getParameterTypes.toSeq
    val realArgs = FunctionalInterfaceAdapter.performSAMTransform(args, paramTypes)
    matching.newInstance(realArgs.map(_.asInstanceOf[AnyRef])*)
  }

  inline def getTypeNames(xs: Seq[Any]): Seq[String] = xs.map {
    case null => "null"
    case cls => cls.getClass.getName
  }

  private def findMatchingConstructor(constructors: Seq[Constructor[?]], args: Seq[Any]): Option[Constructor[?]] =
    val matching = constructors.filter(_.getParameterCount == args.length) match
      case xs if xs.length <= 1 => xs
      case xs => xs.filter(c => DotAccessor.checkTypeFits(c.getParameterTypes.toSeq)(args))
    matching.headOption

  val ConstructorEnvironment: Environments.Env = Environments.EmptyEnv.withValues(Seq(
    "new" -> PrimitiveMacro {
      case (LispExp.Symbol(sym) :: args, env) =>
        Evaluator.evalList(args, env) match
          case Right(exps) =>
            env.getValueOption(sym) match
              case Some(LispExp.WrappedScalaObject(clazz: Class[?])) =>
                fromScalaNative(newInstanceForClass(clazz, exps.map(toScalaNative))) -> env
              case _ =>
                fromScalaNative(newInstanceFromClassName(sym, exps.map(toScalaNative))) -> env
          case Left(reason) => throw new RuntimeException(reason)
    }.withDocString("General Constructor")
  ))
end ConstructorCaller
