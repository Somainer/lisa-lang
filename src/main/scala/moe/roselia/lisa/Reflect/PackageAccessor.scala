package moe.roselia.lisa.Reflect

import scala.reflect.ClassTag
import scala.util.Try
import moe.roselia.lisa.Environments.*
import moe.roselia.lisa.LispExp.*
import ScalaBridge.*
import moe.roselia.lisa.Annotation.RawLisa

object PackageAccessor:
  // In Scala 3, the ToolBox-based rootScalaEnv is replaced with a Java reflection-based approach
  private[lisa] val rootScalaEnv = new SpecialEnv:
    override def getValueOption(key: String): Option[Expression] =
      // Try to resolve as a static field or class
      Try {
        val lastDot = key.lastIndexOf('.')
        if lastDot > 0 then
          val className = key.substring(0, lastDot)
          val fieldName = key.substring(lastDot + 1)
          val clazz = Class.forName(className)
          StaticFieldAccessor.getFieldOrNilArityMethod(clazz, fieldName)
        else
          Class.forName(key)
      }.toOption.map(fromScalaNative)

  case class ObjectEnv[T: ClassTag](obj: T) extends SpecialEnv:
    private lazy val clazz = obj.getClass
    private lazy val shouldPassRawExpression = DotAccessor.hasRawLisaAnnotation(clazz)

    private def mapRealArguments(xs: Seq[Expression]): Seq[Any] =
      if shouldPassRawExpression then xs
      else xs.map(toScalaNative)

    override def getValueOption(key: String): Option[Expression] =
      Try[Expression] {
        val methods = DotAccessor.lookUpForMethod(clazz, key)
        val nilArityMethod = methods.find(_.getParameterCount == 0)
        if nilArityMethod.isDefined then
          nilArityMethod.map { m =>
            m.setAccessible(true)
            m.invoke(obj)
          }.map(fromScalaNative).get
        else PrimitiveFunction { xs =>
          fromScalaNative(DotAccessor.applyDot(key)(obj)(mapRealArguments(xs)*)(xs*))
        }
      }.toOption

  object ObjectEnv:
    def cachedOf[T: ClassTag](obj: T): SpecialEnv = SpecialEnv.cached(ObjectEnv(obj))

    def withKeyTransformer(env: Environment, transformer: String => String): SpecialEnv = new SpecialEnv:
      override def getValueOption(key: String): Option[Expression] =
        env.getValueOption(transformer(key))

      override def has(key: String): Boolean = env.has(transformer(key))

    def ofKebabCase[T: ClassTag](obj: T): SpecialEnv =
      withKeyTransformer(ObjectEnv(obj), s => {
        val splitByHyphen = s.split("-")
        splitByHyphen.head + splitByHyphen.tail.map(_.capitalize).mkString
      })
end PackageAccessor
