package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._

import scala.collection.StringOps

object DotAccessor {
  import reflect.runtime.universe._

  def getTypeTag[T: TypeTag](t: T) = typeOf[T]

  def getClassObject[A : ClassTag](obj: A) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    mirror.reflect(obj)
  }

  @throws[ScalaReflectionException]("If no such field or 0-arity method")
  def accessDot[A : ClassTag](acc: String)(obj: A) = {
    val classObj = getClassObject(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
    if(decl.isTerm) {
      decl.asTerm.alternatives
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
        .getOrElse(classObj.reflectField(decl.asTerm.accessed.asTerm).get)
    } else throw ScalaReflectionException(s"Field or 0-arity method $acc for $obj not found")
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

  @throws[ScalaReflectionException]("When no underlying method")
  def applyDot[A : ClassTag](acc: String)(obj: A)(args: Any*) = {
    val classObj = getClassObject(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
    if(!decl.isTerm) throw ScalaReflectionException(s"No such method $acc in $obj to apply")
    val overloads = decl.asTerm.alternatives
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(_.paramLists.head.length == args.length) match {
        case xs@_::Nil => xs
        case xs => xs.filter(sig => checkTypeFits(sig.paramLists.head)(args.toSeq))
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

  lazy val accessEnv: SpecialEnv = new SpecialEnv {
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x::Nil =>
        val field = key.substring(1)
        field.toIntOption.map(toScalaNative(x).asInstanceOf[Seq[Any]](_)).map(fromScalaNative).getOrElse {
          val res = accessDot(field)(toScalaNative(x))
          fromScalaNative(res)
        }
      case x::xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative): _*))
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a scala object. " +
      s"If you do not care about performance, use box$key"))
  }
}
