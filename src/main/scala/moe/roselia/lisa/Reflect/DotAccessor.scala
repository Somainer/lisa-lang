package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._

object DotAccessor {
  import reflect.runtime.universe._

  def accessDot[A: TypeTag : ClassTag](acc: String)(obj: A) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    val classObj = mirror.reflect(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
    if(decl.isMethod) {
      val meth = classObj.reflectMethod(decl.asMethod)
      meth.symbol.paramLists match {
        case Nil => meth.apply()
        case List(Nil) => meth.apply()
        case _ => (xs: Any) => meth.apply(xs)
      }
    }
    else classObj.reflectField(decl.asTerm).get
  }

  def checkTypeFits(sym: List[Symbol])(args: Seq[Any]) = {
    def checkType[T: TypeTag: ClassTag](t: T)(typ: Type) = {
      val cls = t.getClass
      val mirror = runtimeMirror(cls.getClassLoader)
      val clsObj = mirror.reflect(t)
      println(s"$t T: ${mirror.runtimeClass(clsObj.symbol.toType)}")
      clsObj.symbol.toType weak_<:< typ
    }
    sym.zip(args).forall {
      case (x, y) => checkType(y)(x.typeSignature)
    }
  }

  def applyDot[A: TypeTag : ClassTag](acc: String)(obj: A)(args: Any*) = {
    val cls = obj.getClass
    val mirror = runtimeMirror(cls.getClassLoader)
    val classObj = mirror.reflect(obj)
    val decl = classObj.symbol.toType.decl(TermName(acc))
    val overloads = decl.asTerm.alternatives
      .filter(_.isMethod)
      .map(_.asMethod)
      .filter(_.paramLists.head.length == args.length)
      .filter(sig => checkTypeFits(sig.paramLists.head)(args.toSeq))
    println(decl.asTerm.alternatives.filter(_.isMethod).map(_.asMethod)
      .filter(_.paramLists.head.length == args.length).map(pam => {
        val pm = pam.paramLists.head.map(_.typeSignature).map(_.toString).mkString(", ")
        s"($pm) => ${pam.returnType.toString}"
      }))
    println(s"$acc has ${overloads.length} overloads and received ${args.length} args")
    println(overloads)
    classObj.reflectMethod(decl.asMethod).apply(args: _*)
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
