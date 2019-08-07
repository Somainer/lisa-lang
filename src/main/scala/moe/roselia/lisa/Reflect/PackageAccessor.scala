package moe.roselia.lisa.Reflect
import scala.tools.reflect.ToolBox
import moe.roselia.lisa.Environments._
import moe.roselia.lisa.LispExp._
import ScalaBridge._

import scala.reflect.ClassTag
import scala.util.Try

object PackageAccessor {
  private lazy val toolBox = scala.reflect.runtime.currentMirror.mkToolBox()
  private[lisa] val rootScalaEnv = new SpecialEnv {
    override def getValueOption(key: String): Option[Expression] =
      Try {toolBox.eval(toolBox.parse(key))}.toOption.map(fromScalaNative)
  }

  import scala.reflect.runtime.universe.{runtimeMirror, TermName}
  case class ObjectEnv[T : ClassTag](obj: T) extends SpecialEnv {
    private lazy val toolBox = {
      val cls = obj.getClass
      val mirror = runtimeMirror(cls.getClassLoader)
      val box = mirror.mkToolBox()
      box
    }

    override def getValueOption(key: String): Option[Expression] =
      Try {toolBox.eval(toolBox.parse(key))}.orElse(Try{
        DotAccessor.accessDot(key)(obj)
      }).map(fromScalaNative).orElse(Try[Expression] {
        val clsObj = toolBox.mirror.reflect(obj)
        val name = TermName(key)
        val decl = clsObj.symbol.toType.decl(name)
        if (decl.isMethod) PrimitiveFunction {
          xs => fromScalaNative(DotAccessor.applyDot(key)(obj)(xs: _*))
        }
        else throw new NoSuchMethodException(s"$name is not a method")
      }).toOption
  }


}
