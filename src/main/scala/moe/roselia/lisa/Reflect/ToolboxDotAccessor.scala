/*
* ToolboxDotAccessor can ensure a more reliable access env, but slower.
* */
package moe.roselia.lisa.Reflect
import language.experimental.macros
import scala.reflect.ClassTag
import scala.tools.reflect.ToolBox
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge._


object ToolboxDotAccessor {
  import reflect.runtime.universe._

  private lazy val box = scala.reflect.runtime.currentMirror.mkToolBox()

  def accessDot[A : ClassTag](acc: String)(obj: A) = {
    val constant = Constant(obj)
    val selected = TermName(acc)
    box.eval(q"$constant.$selected")
  }

  def applyDot[A : ClassTag](acc: String)(obj: A)(args: Any*) = {
    val arguments = args.map(Constant(_))
    val funcTerm = TermName(acc)
    val selector = Constant(obj)
    box.eval(
      q"""
         $selector.$funcTerm(..$arguments)
       """)
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
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a scala object, using scala Toolbox." +
      s"Slower but you will get a more precise error information when encountering exceptions."))
  }
}
