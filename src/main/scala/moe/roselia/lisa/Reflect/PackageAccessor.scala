package moe.roselia.lisa.Reflect
import scala.tools.reflect.ToolBox
import moe.roselia.lisa.Environments._
import moe.roselia.lisa.LispExp._
import ScalaBridge._
import moe.roselia.lisa.Annotation.RawLisa

import scala.reflect.ClassTag
import scala.util.Try

object PackageAccessor {
  private lazy val toolBox = scala.reflect.runtime.currentMirror.mkToolBox()
  private[lisa] val rootScalaEnv = new SpecialEnv {
    override def getValueOption(key: String): Option[Expression] =
      Try {toolBox.eval(toolBox.parse(key))}.toOption.map(fromScalaNative)
  }

  case class ObjectEnv[T : ClassTag](obj: T) extends SpecialEnv {
    private lazy val classMirror = DotAccessor.getClassObject(obj)
    private lazy val shouldPassRawExpression = DotAccessor.hasRawLisaAnnotation(classMirror.symbol)

    private def mapRealArguments(xs: Seq[Expression]): Seq[Any] = {
      if (shouldPassRawExpression) xs
      else xs.map(toScalaNative)
    }

    override def getValueOption(key: String): Option[Expression] =
      Try[Expression] {
        val clsObj = classMirror
//        val decl = clsObj.symbol.toType.decl(name)
        val decl = DotAccessor.lookUpForTerm(clsObj.symbol, key)
        if (decl.isDefined) {
          val nilArityMethod = decl.get.alternatives
            .filter(_.isMethod)
            .map(_.asMethod)
            .find(_.paramLists == Nil)
            .map(clsObj.reflectMethod)
          if (nilArityMethod.isDefined) nilArityMethod.map(_.apply()).map(fromScalaNative).get
          else PrimitiveFunction {
            xs => fromScalaNative(DotAccessor.applyDot(key)(obj)(mapRealArguments(xs): _*))
          }
        }
        else throw ScalaReflectionException(s"$key is not a method")
      }.toOption
  }

  object ObjectEnv {
    def cachedOf[T : ClassTag](obj: T) = SpecialEnv.cached(ObjectEnv(obj))

    def withKeyTransformer(env: Environment, transformer: String => String): SpecialEnv = new SpecialEnv {
      override def getValueOption(key: String): Option[Expression] =
        env.getValueOption(transformer(key))

      override def has(key: String): Boolean = env.has(transformer(key))
    }

    def ofKebabCase[T : ClassTag](obj: T): SpecialEnv =
      withKeyTransformer(ObjectEnv(obj), s => {
        val splitByHyphen = s.split("-")
        splitByHyphen.head + splitByHyphen.tail.map(_.capitalize).mkString
      })
  }

}
