package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._
import moe.roselia.lisa.LispExp.{LisaList, WrappedScalaObject}
import moe.roselia.lisa.Util.Extractors.NeedInt

object DotAccessor {
  import reflect.runtime.universe._

  def getTypeTag[T: TypeTag](t: T) = typeOf[T]

  def getClassObject[A : ClassTag](obj: A) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    mirror.reflect(obj)
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
        .getOrElse(classObj.reflectField(decl.get.accessed.asTerm).get)
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
        accessDotOfPlainObject(acc)(obj)
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
      clsObj.symbol.toType <:< typ
    }
    sym.zip(args).forall {
      case (x, y) => checkType(y)(x.typeSignature) || checkType(y)(x.typeSignature.map(boxedType))
    }
  }

  def lookUpForTerm(symbol: ClassSymbol, accessName: String) = {
    (symbol :: symbol.baseClasses).view
      .map(_.asClass)
      .map(_.toType)
      .map(_.decl(TermName(toJvmName(accessName))))
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

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDotOfPlainObject[A : ClassTag](acc: String)(obj: A)(args: Any*) = {
    val classObj = getClassObject(obj)
//    val decl = classObj.symbol.toType.decl(TermName(acc))
    val decl = lookUpForTerm(classObj.symbol, acc)
    if(decl.isEmpty) throw ScalaReflectionException(s"No such method $acc in $obj: ${classObj.symbol.fullName} to apply")
    val overloads = decl.get.alternatives
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(_.paramLists.flatten.length == args.length) match {
        case xs@_::Nil => xs
        case xs => xs.filter(sig => checkTypeFits(sig.paramLists.flatten)(args.toSeq))
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
    classObj.reflectMethod(overloads.head).apply(args: _*)
  }

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDot[A : ClassTag](acc: String)(obj: A)(args: Any*) = {
    obj match {
      case dynamic: Dynamic => util.Try {
        applyDotDynamic(acc)(dynamic)(args: _*)
      }.getOrElse(applyDotOfPlainObject(acc)(obj)(args: _*))
      case _ =>
        applyDotOfPlainObject(acc)(obj)(args: _*)
    }
  }

  lazy val accessEnv: SpecialEnv = SpecialEnv.cached(new SpecialEnv {
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x::Nil =>
        key.substring(1) match {
          case s"[${NeedInt(index)}]" => x match {
            case LisaList(ll) => ll(index)
            case WrappedScalaObject(seq: Seq[_]) => fromScalaNative(seq(index))
            case _ => fromScalaNative(toScalaNative(x).asInstanceOf[Seq[Any]](index))
          }
          case field =>
            val res = accessDot(field)(toScalaNative(x))
            fromScalaNative(res)
        }
      case x::xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative): _*))
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a scala object. " +
      s"If you do not care about performance, use box$key"))
  })
}
