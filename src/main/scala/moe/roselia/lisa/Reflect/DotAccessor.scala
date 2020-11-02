package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._
import moe.roselia.lisa.Annotation.RawLisa
import moe.roselia.lisa.LispExp.{Expression, LisaList, Procedure, WrappedScalaObject}
import moe.roselia.lisa.Util.Extractors.NeedInt

object DotAccessor {
  import reflect.runtime.universe._

  def getTypeTag[T: TypeTag](t: T) = typeOf[T]

  def getClassObject[A : ClassTag](obj: A) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    mirror.reflect(obj)
  }

  def hasRawLisaAnnotation(symbol: Symbol): Boolean = {
    symbol.annotations.view.map(_.tree.tpe).exists(_ <:< typeOf[RawLisa])
  }

  def accessDotDynamic(acc: String)(dyn: Dynamic): Any = {
    import scala.language.reflectiveCalls

    dyn.asInstanceOf[{
      def selectDynamic(key: Any): Any
    }].selectDynamic(acc)
  }

  @throws[ScalaReflectionException]("If no such field or 0-arity method")
  def accessDotOfPlainObject[A : ClassTag](acc: String)(obj: A) = {
    val classObj = getClassObject(obj)
    val decl = lookUpForTerm(classObj.symbol, acc)
//    val decl = classObj.symbol.toType.decl(TermName(acc))
    if(decl.isDefined) {
      decl.get.alternatives
        .filter(_.isMethod)
        .map(_.asMethod)
        .find(_.paramLists match {
          case Nil => true
          case Nil::Nil => true
          case _ => false
        })
        .map(_.asMethod)
        .map(classObj.reflectMethod)
        .map(_.apply())
        .getOrElse(classObj.reflectField(util.Try(decl.get.accessed.asTerm).getOrElse(decl.get)).get)
    } else throw ScalaReflectionException(s"Field or 0-arity method $acc for $obj: ${classObj.symbol.fullName} not found")
  }

  @throws[ScalaReflectionException]("If no such field or 0-arity method")
  def accessDot[A : ClassTag](acc: String)(obj: A) = {
    obj match {
      case dynamic: Dynamic =>
        util.Try {
          accessDotDynamic(acc)(dynamic)
        }.getOrElse(accessDotOfPlainObject(acc)(obj))
      case _ =>
        handleReflectionException(accessDotOfPlainObject(acc)(obj))
    }
  }

  def checkTypeFits(sym: List[Symbol])(args: Seq[Any]): Boolean = {
    def boxedType(tpe: Type): Type = tpe match {
      case a if a =:= typeOf[Int] => getTypeTag(Int.box(0))
      case a if a =:= typeOf[Boolean] => getTypeTag(Boolean.box(false))
      case a if a =:= typeOf[Double] => getTypeTag(Double.box(0))
      case a if a =:= typeOf[Float] => getTypeTag(Float.box(0))
      case a if a =:= typeOf[Char] => getTypeTag(Char.box(0))
      case a if a =:= typeOf[Short] => getTypeTag(Short.box(0))
      case a if a =:= typeOf[Byte] => getTypeTag(Byte.box(0))
      case a if a =:= typeOf[Long] => getTypeTag(Long.box(0))
      case a => a
    }

    def checkType[T: TypeTag: ClassTag](t: T)(typ: Type) = {
      val cls = t.getClass
      val mirror = runtimeMirror(cls.getClassLoader)
      val clsObj = mirror.reflect(t)
//      println(s"$t T: ${mirror.runtimeClass(clsObj.symbol.toType)}")
//      println(s"${clsObj.symbol.toType} <:< $typ: ${clsObj.symbol.toType <:< typ}")
      if (FunctionalInterfaceAdapter.isFunctional(cls)) {
        t.isInstanceOf[Procedure] || clsObj.symbol.toType <:< typ
      } else {
        clsObj.symbol.toType <:< typ
      }
    }
    sym.lengthIs == args.length && sym.zip(args).forall {
      case (x, y) => null == y || checkType(y)(x.typeSignature) || checkType(y)(x.typeSignature.map(boxedType))
    }
  }

  def performSAMTransform(sym: List[Symbol])(args: Seq[Any]): Seq[Any] = {
    if (sym.length != args.length) args // If their lengths are not equal then do nothing.
    else {
      args.indices.map { i =>
        val arg = args(i)
        val symbol = sym(i).typeSignature.typeSymbol
        arg match {
          case procedure: Procedure if symbol.isAbstract =>
            val mirror = runtimeMirror(arg.getClass.getClassLoader)
            val clazz = mirror.runtimeClass(symbol.asClass)
            if (FunctionalInterfaceAdapter.isFunctional(clazz)) {
              FunctionalInterfaceAdapter.createFunctionInvoker(clazz, procedure)
            } else arg
          case _ => arg
        }
      }
    }
  }

  def lookUpForTerm(symbol: ClassSymbol, accessName: String) = {
    val termName = TermName(toJvmName(accessName))
    (symbol :: symbol.baseClasses).view
      .map(_.asClass)
      .map(_.toType)
      .map(_.decl(termName))
      .find(_.isTerm)
      .map(_.asTerm)
  }

  def toJvmName(name: String) = {
    reflect.NameTransformer.encode(name)
  }

  def applyDotDynamic(acc: String)(dyn: Dynamic)(args: Any*) = {
    import scala.language.reflectiveCalls

    dyn.asInstanceOf[{
      def applyDynamic(key: Any)(args: Any*): Any
    }].applyDynamic(acc)(args: _*)
  }

  def handleReflectionException[T](block: => T): T = {
    try block
    catch {
      case ex: java.lang.reflect.InvocationTargetException =>
        throw ex.getTargetException
    }
  }

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDotOfPlainObject[A : ClassTag](acc: String)(obj: A)(args: Any*)(expressionArgs: Expression*) = {
    val classObj = getClassObject(obj)
//    val decl = classObj.symbol.toType.decl(TermName(acc))
    val decl = lookUpForTerm(classObj.symbol, acc)
    if(decl.isEmpty) throw ScalaReflectionException(s"No such method $acc in $obj: ${classObj.symbol.fullName} to apply")
    val overloads = decl.get.alternatives
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(overload => {
        if(hasRawLisaAnnotation(overload)) overload.paramLists.flatten.length == expressionArgs.length
        else overload.paramLists.flatten.length == args.length
      }) match {
        case xs@_::Nil => xs
        case xs =>
          val (rawNames, native) = xs.partition(hasRawLisaAnnotation)
          rawNames.filter(sig => checkTypeFits(sig.paramLists.flatten)(expressionArgs)) ++
            native.filter(sig => checkTypeFits(sig.paramLists.flatten)(args))
      }

//    println(decl.asTerm.alternatives.filter(_.isMethod).map(_.asMethod)
//      .filter(_.paramLists.head.length == args.length).map(pam => {
//        val pm = pam.paramLists.head.map(_.typeSignature).map(_.toString).mkString(", ")
//        s"($pm) => ${pam.returnType.toString}"
//      }))
//    println(s"$acc has ${overloads.length} overloads and received ${args.length} args")
//    println(classObj.symbol)
//    println(overloads)
    if (overloads.isEmpty) throw ScalaReflectionException(s"No overloaded method $acc in $obj to apply")
    overloads.head match {
      case symbol if hasRawLisaAnnotation(symbol) =>
        classObj.reflectMethod(symbol).apply(expressionArgs: _*)
      case symbol =>
        val samArgs = performSAMTransform(symbol.paramLists.flatten)(args)
        classObj.reflectMethod(symbol).apply(samArgs: _*)
    }
  }

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDot[A : ClassTag](acc: String)(obj: A)(args: Any*)(expressionArgs: Expression*) = {
    obj match {
      case dynamic: Dynamic => util.Try {
        applyDotDynamic(acc)(dynamic)(args: _*)
      }.getOrElse(applyDotOfPlainObject(acc)(obj)(args: _*)(expressionArgs: _*))
      case _ =>
        handleReflectionException(applyDotOfPlainObject(acc)(obj)(args: _*)(expressionArgs: _*))
    }
  }

  lazy val accessEnv: SpecialEnv = SpecialEnv.cached(new SpecialEnv {
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x::Nil =>
        key.substring(1) match {
          case s"[${NeedInt(index)}]" => x match {
            case LisaList(ll) => ll(index)
            case WrappedScalaObject(array: Array[_]) => fromScalaNative(array(index))
            case WrappedScalaObject(seq: Seq[_]) => fromScalaNative(seq(index))
            case _ => fromScalaNative(toScalaNative(x).asInstanceOf[Seq[Any]](index))
          }
          case field =>
            val res = accessDot(field)(toScalaNative(x))
            fromScalaNative(res)
        }
      case x::xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative): _*)(xs: _*))
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a jvm object."))
  })
}
