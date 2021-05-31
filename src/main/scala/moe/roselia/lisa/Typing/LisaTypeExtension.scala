package moe.roselia.lisa.Typing
import scala.reflect.runtime.{universe => ru}

trait LisaTypeExtension {
  import ru._

  @annotation.tailrec
  final def typeToSymbol(lisaType: LisaType): Type = lisaType match {
    case _: AnyType => typeOf[Any]
    case _: BottomType => typeOf[Nothing]
    case LisaType.nil => typeOf[Unit]
    case scalaSymbolClassType: ScalaSymbolClassType[_] =>
      scalaSymbolClassType.scalaType
    case classType: ClassTypeLike[_] =>
      val mirror = ru.runtimeMirror(classType.runtimeClass.getClassLoader)
      mirror.reflectClass(mirror.classSymbol(classType.runtimeClass)).symbol.asType.toType
    case LiteralType(_, underlyingType) => typeToSymbol(underlyingType)
    case tpe => throw new UnsupportedOperationException(s"Cannot get type symbol of $tpe")
  }

  def symbolToType(scalaType: Type): LisaType = scalaType.dealias match {
    case x if x =:= typeOf[Any] => LisaType.any
    case x if x =:= typeOf[Nothing] => LisaType.nothing
    case x if x =:= typeOf[Unit] => LisaType.nil
    case x if x.typeSymbol.isClass =>
      ScalaSymbolType(x)
    case _ => throw new UnsupportedOperationException(s"Cannot convert symbol $scalaType to type")
  }
}
object LisaTypeExtension extends LisaTypeExtension { self =>
  implicit class LisaTypeExt(val lisaType: LisaType) extends AnyVal {
    def scalaSymbol: ru.Type = self.typeToSymbol(lisaType)
  }

  implicit class ScalaTypeExt(val scalaType: ru.Type) extends AnyVal {
    def toLisaType: LisaType = self.symbolToType(scalaType)
  }
}
