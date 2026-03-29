package moe.roselia.lisa.Reflect

import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge.*
import moe.roselia.lisa.Annotation.RawLisa
import moe.roselia.lisa.LispExp.{Expression, LisaList, Procedure, WrappedScalaObject}
import moe.roselia.lisa.Util.Extractors.NeedInt

import java.lang.reflect.{InvocationTargetException, Method, Modifier}

object DotAccessor:

  def hasRawLisaAnnotation(method: Method): Boolean =
    method.isAnnotationPresent(classOf[RawLisa])

  def hasRawLisaAnnotation(clazz: Class[?]): Boolean =
    clazz.isAnnotationPresent(classOf[RawLisa])

  private def isAccessible(member: java.lang.reflect.Member): Boolean =
    Modifier.isPublic(member.getModifiers)

  def toJvmName(name: String): String =
    scala.reflect.NameTransformer.encode(name)

  def lookUpForMethod(clazz: Class[?], accessName: String): Seq[Method] =
    val jvmName = toJvmName(accessName)
    val methods = collectMethods(clazz)
    methods.filter(m => m.getName == jvmName || m.getName == accessName)

  private def collectMethods(clazz: Class[?]): Seq[Method] =
    if clazz == null then Nil
    else
      val own = clazz.getDeclaredMethods.toSeq
      val parent = collectMethods(clazz.getSuperclass)
      val interfaces = clazz.getInterfaces.flatMap(collectMethods).toSeq
      own ++ parent ++ interfaces

  def lookUpForField(clazz: Class[?], accessName: String): Option[java.lang.reflect.Field] =
    val jvmName = toJvmName(accessName)
    try
      Some(clazz.getField(jvmName))
    catch
      case _: NoSuchFieldException =>
        try Some(clazz.getField(accessName))
        catch case _: NoSuchFieldException => None

  def accessDotDynamic(acc: String)(dyn: Dynamic): Any =
    import scala.language.reflectiveCalls
    dyn.asInstanceOf[{
      def selectDynamic(key: Any): Any
    }].selectDynamic(acc)

  @throws[ScalaReflectionException]("If no such field or 0-arity method")
  def accessDotOfPlainObject(acc: String)(obj: Any): Any =
    val clazz = obj.getClass
    val methods = lookUpForMethod(clazz, acc)

    // Try 0-arity methods first
    val nilArityMethod = methods.find(_.getParameterCount == 0)
    nilArityMethod match
      case Some(method) =>
        method.setAccessible(true)
        handleReflectionException(method.invoke(obj))
      case None =>
        // Try field access
        lookUpForField(clazz, acc) match
          case Some(field) =>
            field.setAccessible(true)
            field.get(obj)
          case None =>
            throw ScalaReflectionException(
              s"Field or 0-arity method $acc for $obj: ${clazz.getName} not found")

  @throws[ScalaReflectionException]("If no such field or 0-arity method")
  def accessDot[A: ClassTag](acc: String)(obj: A): Any =
    obj match
      case dynamic: Dynamic =>
        util.Try(accessDotDynamic(acc)(dynamic))
          .getOrElse(accessDotOfPlainObject(acc)(obj))
      case _ =>
        accessDotOfPlainObject(acc)(obj)

  def checkTypeFits(paramTypes: Seq[Class[?]])(args: Seq[Any]): Boolean =
    paramTypes.length == args.length && paramTypes.zip(args).forall { (paramType, arg) =>
      arg == null ||
        StaticFieldAccessor.jvmBoxedClass(paramType).isInstance(arg) ||
        (arg.isInstanceOf[Procedure] && FunctionalInterfaceAdapter.isFunctional(paramType))
    }

  def performSAMTransform(paramTypes: Seq[Class[?]])(args: Seq[Any]): Seq[Any] =
    if paramTypes.length != args.length then args
    else
      args.indices.map { i =>
        val arg = args(i)
        val paramType = paramTypes(i)
        arg match
          case _ if paramType == classOf[Any] || paramType == classOf[Object] => arg
          case procedure: Procedure if Modifier.isAbstract(paramType.getModifiers) &&
            FunctionalInterfaceAdapter.isFunctional(paramType) =>
            FunctionalInterfaceAdapter.createFunctionInvoker(paramType, procedure)
          case _ => arg
      }

  def getDeclaredTermNames(clazz: Class[?]): Seq[String] =
    val methods = collectMethods(clazz).map(_.getName).map(scala.reflect.NameTransformer.decode)
    val fields = Option(clazz).map(_.getFields.map(_.getName).toSeq).getOrElse(Nil)
    (methods ++ fields).distinct

  def applyDotDynamic(acc: String)(dyn: Dynamic)(args: Any*): Any =
    // Use Java reflection to call applyDynamic since Scala 3 doesn't allow _* in structural type calls
    val clazz = dyn.getClass
    val methods = clazz.getMethods.filter(m => m.getName == "applyDynamic" && m.getParameterCount == 2)
    methods.headOption match
      case Some(method) =>
        method.setAccessible(true)
        method.invoke(dyn, acc, args.toArray)
      case None =>
        throw ScalaReflectionException(s"applyDynamic not found on ${clazz.getName}")

  def handleReflectionException[T](block: => T): T =
    try block
    catch
      case ex: InvocationTargetException =>
        throw ex.getTargetException

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDotOfPlainObject(acc: String)(obj: Any)(args: Any*)(expressionArgs: Expression*): Any =
    val clazz = obj.getClass
    val methods = lookUpForMethod(clazz, acc)

    if methods.isEmpty then
      throw ScalaReflectionException(s"No such method $acc in $obj: ${clazz.getName} to apply")

    val overloads = methods.filter { method =>
      if hasRawLisaAnnotation(method) then method.getParameterCount == expressionArgs.length
      else method.getParameterCount == args.length
    } match
      case xs if xs.length <= 1 => xs
      case xs =>
        val (rawMethods, nativeMethods) = xs.partition(hasRawLisaAnnotation)
        val matchingRaw = rawMethods.filter(m => checkTypeFits(m.getParameterTypes.toSeq)(expressionArgs))
        val matchingNative = nativeMethods.filter(m => checkTypeFits(m.getParameterTypes.toSeq)(args))
        matchingRaw ++ matchingNative

    if overloads.isEmpty then
      throw ScalaReflectionException(s"No overloaded method $acc in $obj to apply")

    val method = overloads.head
    method.setAccessible(true)
    if hasRawLisaAnnotation(method) then
      handleReflectionException(method.invoke(obj, expressionArgs.map(_.asInstanceOf[AnyRef])* ))
    else
      val samArgs = performSAMTransform(method.getParameterTypes.toSeq)(args)
      handleReflectionException(method.invoke(obj, samArgs.map(_.asInstanceOf[AnyRef])*))

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDot[A: ClassTag](acc: String)(obj: A)(args: Any*)(expressionArgs: Expression*): Any =
    obj match
      case dynamic: Dynamic =>
        util.Try(applyDotDynamic(acc)(dynamic)(args*))
          .getOrElse(applyDotOfPlainObject(acc)(obj)(args*)(expressionArgs*))
      case _ =>
        handleReflectionException(applyDotOfPlainObject(acc)(obj)(args*)(expressionArgs*))

  lazy val accessEnv: SpecialEnv = SpecialEnv.cached(new SpecialEnv:
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x :: Nil =>
        key.substring(1) match
          case s"[${NeedInt(index)}]" => x match
            case LisaList(ll) => ll(index)
            case WrappedScalaObject(array: Array[?]) => fromScalaNative(array(index))
            case WrappedScalaObject(seq: Seq[?]) => fromScalaNative(seq(index))
            case _ => fromScalaNative(toScalaNative(x).asInstanceOf[Seq[Any]](index))
          case field =>
            val res = accessDot(field)(toScalaNative(x))
            fromScalaNative(res)
      case x :: xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative)*)(xs*))
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a jvm object."))
  )
end DotAccessor
