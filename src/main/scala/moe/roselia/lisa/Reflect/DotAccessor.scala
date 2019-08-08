package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import scala.tools.reflect.ToolBox
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._

import scala.collection.StringOps

object DotAccessor {
  import reflect.runtime.universe._

  def accessDot[A : ClassTag](acc: String)(obj: A) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    val classObj = mirror.reflect(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
//    println(cls.getDeclaredFields.map(_.getName).mkString(", "))
//    println(cls.getDeclaredMethods.map(_.getName).mkString(", "))
    if(decl.isMethod) {
      val meth = classObj.reflectMethod(decl.asMethod)
      meth.apply()
//      meth.symbol.paramLists match {
//        case e => meth.apply()
//      }
    }
    else classObj.reflectField(decl.asTerm).get
  }

  def checkTypeFits(sym: List[Symbol])(args: Seq[Any]) = {
    def checkType[T: TypeTag: ClassTag](t: T)(typ: Type) = {
      val cls = t.getClass
      val mirror = runtimeMirror(cls.getClassLoader)
      val clsObj = mirror.reflect(t)
//      println(s"$t T: ${mirror.runtimeClass(clsObj.symbol.toType)}")
//      println(s"${clsObj.symbol.toType} <:< $typ: ${clsObj.symbol.toType <:< typ}")
      clsObj.symbol.toType <:< typ
    }
    sym.zip(args).forall {
      case (x, y) => checkType(y)(x.typeSignature) || checkType(sugaredObject(y))(x.typeSignature)
    }
  }

  def sugaredObject(obj: Any): Any = obj match {
    case i: java.lang.Integer => i.intValue()
    case d: java.lang.Double => d.doubleValue()
    case f: java.lang.Float => f.floatValue()
    case b: java.lang.Boolean => b.booleanValue()
    case s: java.lang.String => new StringOps(s)
    case els => els
  }

  def applyDot[A : ClassTag](acc: String)(obj: A)(args: Any*) = {
//    val obj = sugaredObject(rawObj)
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
//    val box = scala.reflect.runtime.currentMirror.mkToolBox()
    val classObj = mirror.reflect(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
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
    classObj.reflectMethod(overloads(0)).apply(args: _*)
//    classObj.reflectMethod(decl.asMethod).apply(args: _*)
//    box.eval(
//      q"""
//         $decl($applied)
//       """)
  }

  val accessEnv = new SpecialEnv {
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x::Nil => {
        val field = key.substring(1)
        field.toIntOption.map(toScalaNative(x).asInstanceOf[Seq[Any]](_)).map(fromScalaNative).getOrElse {
          val res = accessDot(field)(toScalaNative(x))
          fromScalaNative(res)
        }
      }
      case x::xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative): _*))
    })
  }
}
