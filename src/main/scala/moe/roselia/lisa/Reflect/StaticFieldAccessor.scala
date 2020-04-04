package moe.roselia.lisa.Reflect

import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import moe.roselia.lisa.Util.Extractors.RichOption
import ScalaBridge.{fromScalaNative, toScalaNative}
import moe.roselia.lisa.LispExp.{Expression, PrimitiveFunction}

object StaticFieldAccessor {
  def jvmBoxedClass(clazz: Class[_]): Class[_] = clazz match {
    case a if a == classOf[Int] => classOf[java.lang.Integer]
    case a if a == classOf[Boolean] => classOf[java.lang.Boolean]
    case a if a == classOf[Float] => classOf[java.lang.Float]
    case a if a == classOf[Double] => classOf[java.lang.Double]
    case a if a == classOf[Char] => classOf[java.lang.Character]
    case a if a == classOf[Short] => classOf[java.lang.Short]
    case a if a == classOf[Byte] => classOf[java.lang.Byte]
    case a if a == classOf[Long] => classOf[java.lang.Long]
    case a => a
  }

  def memberIsStatic(member: java.lang.reflect.Member): Boolean = {
    (member.getModifiers & java.lang.reflect.Modifier.STATIC) != 0
  }

  @scala.annotation.tailrec
  def getStaticMethodOfClassByName(clazz: Class[_], name: String): Seq[java.lang.reflect.Method] = if (clazz eq null) Nil else {
    clazz.getDeclaredMethods.filter(memberIsStatic).filter(_.getName == name) match {
      case xs if xs.isEmpty => getStaticMethodOfClassByName(clazz.getSuperclass, name)
      case xs => xs
    }
  }

  @scala.annotation.tailrec
  def getStaticFiledOfClassByName(clazz: Class[_], name: String): Option[java.lang.reflect.Field] = {
    if (clazz eq null) None
    else {
      clazz.getDeclaredFields.filter(memberIsStatic).find(_.getName == name) match {
        case None => getStaticFiledOfClassByName(clazz.getSuperclass, name)
        case x => x
      }
    }
  }

  @`inline` def isTypeFitForJava(params: Seq[Class[_]], arguments: Seq[Any]): Boolean =
    params.view.map(jvmBoxedClass).zip(arguments).forall(Function.tupled(_ isInstance _))

  def matchRealArguments(params: Seq[Class[_]], arguments: Seq[Any]): Option[Seq[Any]] = {
    def loop(params: List[Class[_]], arguments: List[Any], buffer: List[Any]): Option[Seq[Any]] = params match {
      case Nil => if(arguments.isEmpty) Some(buffer.reverse) else None
      case arrayTpe :: Nil if arrayTpe.isArray =>
        val underlyingType = jvmBoxedClass(arrayTpe.getComponentType)
        if (arguments.forall(underlyingType.isInstance))
          loop(Nil, Nil, arguments.toArray :: buffer)
        else None
      case cls :: ps => arguments match {
        case x :: xs if cls.isInstance(x) => loop(ps, xs, x :: buffer)
        case _ => None
      }
    }

    loop(params.map(jvmBoxedClass).toList, arguments.toList, Nil)
  }

  def invokeStaticMethodOfClass(clazz: Class[_], name: String)(args: Any*): Option[Any] = DotAccessor.handleReflectionException {
    getStaticMethodOfClassByName(clazz, name).find(method => {
      if(method.isVarArgs) matchRealArguments(method.getParameterTypes, args).isDefined
      else method.getParameterCount == args.length && isTypeFitForJava(method.getParameterTypes, args)
    })
      .filter(_.trySetAccessible())
      .map(method => {
        if (method.isVarArgs) method.invoke(null, matchRealArguments(method.getParameterTypes, args).get: _*)
        else method.invoke(null, args: _*)
      })
  }

  def invokeStaticMethod(clazz: Class[_], name: String)(args: Any*): Any = {
    invokeStaticMethodOfClass(clazz, name)(args: _*)
      .getOrThrow(new NoSuchMethodException(s"No matching static method $name for ${clazz.getName} with types: " +
        s"${ConstructorCaller.getTypeNames(args).mkString("(", ", ", ")")}"))
  }

  def getStaticFieldValueOfClass(clazz: Class[_], name: String): Option[Any] = {
    getStaticFiledOfClassByName(clazz, name)
      .filter(_.trySetAccessible())
      .map(_.get(null))
      // .getOrThrow(new NoSuchFieldException(s"No such filed $name for ${clazz.getName}."))
  }

  def isFieldOrNilArityMethod(clazz: Class[_], name: String): Boolean = {
    getStaticFiledOfClassByName(clazz, name).isDefined ||
      getStaticMethodOfClassByName(clazz, name).exists(_.getParameterCount == 0)
  }

  def isMethod(clazz: Class[_], name: String): Boolean = {
    getStaticMethodOfClassByName(clazz, name).nonEmpty
  }

  def getFieldOrNilArityMethod(clazz: Class[_], name: String): Any = {
    getStaticFieldValueOfClass(clazz, name)
      .orElse(invokeStaticMethodOfClass(clazz, name)())
      .getOrThrow(new NoSuchFieldException(s"No such filed $name for ${clazz.getName}."))
  }


  object StaticFieldsAccessorEnvironment extends SpecialEnv {
    private val cache = collection.mutable.Map.empty[String, Expression]
    private val classCache = collection.mutable.Map.empty[String, Class[_]]

    override def has(key: String): Boolean = cache.contains(key) || scala.util.Try {
      key.split('/') match {
        case Array(className, fieldName) =>
          val clazz = classCache.getOrElseUpdate(className, ConstructorCaller.resolveClassBySimpleName(className))
          isFieldOrNilArityMethod(clazz, fieldName) || isMethod(clazz, fieldName)
        case _ => false
      }
    }.getOrElse(false)

    override def getValueOption(key: String): Option[Expression] = {
      cache.get(key).orElse(getValueByReflection(key))
    }

    private def getValueByReflection(key: String): Option[LispExp.Expression] = {
      key.split('/') match {
        case Array(className, fieldName) => scala.util.Try {
          val clazz = classCache.getOrElseUpdate(className, ConstructorCaller.resolveClassBySimpleName(className))
          if (isFieldOrNilArityMethod(clazz, fieldName)) // We can not cache fields because they are potentially mutable.
            fromScalaNative(getFieldOrNilArityMethod(clazz, fieldName))
          else if(isMethod(clazz, fieldName)) {
            val method = PrimitiveFunction {
              xs => fromScalaNative(invokeStaticMethod(clazz, fieldName)(xs.map(toScalaNative): _*))
            }

            // Methods can be cached because methods are expected to be immutable.
            cache.update(key, method)
            method
          } else throw new NoSuchMethodException(key)
        }.toOption
        case _ => None
      }
    }
  }
}
