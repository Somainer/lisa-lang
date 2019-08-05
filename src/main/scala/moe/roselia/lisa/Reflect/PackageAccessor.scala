package moe.roselia.lisa.Reflect
import scala.tools.reflect.ToolBox
import moe.roselia.lisa.Environments._
import moe.roselia.lisa.LispExp._
import ScalaBridge._

import scala.util.Try

object PackageAccessor {
  private lazy val toolBox = scala.reflect.runtime.currentMirror.mkToolBox()
  val rootScalaEnv = new SpecialEnv {
    override def getValueOption(key: String): Option[Expression] =
      Try {toolBox.eval(toolBox.parse(key))}.toOption.map(fromScalaNative)
  }

  import scala.reflect.runtime.universe.runtimeMirror
  case class ObjectEnv[T](obj: T) extends SpecialEnv {
    lazy val toolBox = {
      val cls = obj.getClass
      val mirror = runtimeMirror(cls.getClassLoader)
      val box = mirror.mkToolBox()
      box
    }

    override def getValueOption(key: String): Option[Expression] =
      Try {toolBox.eval(toolBox.parse(key))}.toOption.map(fromScalaNative)
  }


}
