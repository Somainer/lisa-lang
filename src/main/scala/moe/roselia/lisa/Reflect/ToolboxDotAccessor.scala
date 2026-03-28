/*
 * ToolboxDotAccessor provides an alternative dot accessor using Java reflection.
 * In Scala 3, the original ToolBox-based implementation is replaced with
 * direct Java reflection for broader compatibility.
 */
package moe.roselia.lisa.Reflect

import scala.reflect.ClassTag
import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa.LispExp
import ScalaBridge.*

object ToolboxDotAccessor:

  def accessDot[A: ClassTag](acc: String)(obj: A): Any =
    DotAccessor.accessDot(acc)(obj)

  def applyDot[A: ClassTag](acc: String)(obj: A)(args: Any*): Any =
    DotAccessor.applyDot(acc)(obj)(args*)(Nil*)

  lazy val accessEnv: SpecialEnv = new SpecialEnv:
    override def has(key: String): Boolean = key.startsWith(".")

    override def getValueOption(key: String): Option[LispExp.Expression] = Some(LispExp.PrimitiveFunction {
      case x :: Nil =>
        val field = key.substring(1)
        field.toIntOption.map(toScalaNative(x).asInstanceOf[Seq[Any]](_)).map(fromScalaNative).getOrElse {
          val res = accessDot(field)(toScalaNative(x))
          fromScalaNative(res)
        }
      case x :: xs =>
        val field = key.substring(1)
        fromScalaNative(applyDot(field)(toScalaNative(x))(xs.map(toScalaNative)*))
    }.withDocString(s"$key: Access ${key substring 1} attribute or method of a scala object." +
      s" Uses Java reflection for access."))
end ToolboxDotAccessor
