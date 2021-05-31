package moe.roselia.lisa.Typing

import moe.roselia.lisa.Exceptions.LisaRuntimeException
import moe.roselia.lisa.LispExp.{Expression, IdenticalLisaExpression, InvokableProcedure, LisaListLike, LisaValue}
import moe.roselia.lisa.Reflect.ConstructorCaller
import LisaTypeExtension.{LisaTypeExt, ScalaTypeExt}

import scala.reflect.runtime.universe
import scala.reflect.{ClassTag, classTag}

trait LisaType extends IdenticalLisaExpression with LisaValue with PartiallyOrdered[LisaType] {
  def name: String
  def fullName: String
  def isAssignableTo(other: LisaType): Option[Boolean]
  def isAssignableFrom(other: LisaType): Option[Boolean]

  def <:<(other: LisaType): Boolean =
    this == other || isAssignableTo(other).orElse(other.isAssignableFrom(this)).getOrElse(false)

  def =:=(other: LisaType): Boolean =
    this <:< other && other <:< this

  override def tryCompareTo[B >: LisaType : AsPartiallyOrdered](that: B): Option[Int] = that match {
    case lisaType: LisaType =>
      if (this == lisaType || this =:= lisaType) Some(0)
      else if (this <:< lisaType) Some(-1)
      else if (lisaType <:< this) Some(1)
      else None
    case _ => None
  }

  override def lisaType: LisaType = LisaType.itself
  override def tpe: LisaType = lisaType

  override def toString: String = name
}
object LisaType {
  import JvmValueClass._

  def of[T : ClassTag]: LisaType = of(classTag.runtimeClass)
  def of(cls: Class[_]): LisaType = cls match {
    case NativeByte | ScalaByte | JavaByte => ValueType.Byte
    case NativeShort | ScalaShort | JavaShort => ValueType.Short
    case NativeInt | ScalaInt | JavaInt => ValueType.Int
    case NativeLong | ScalaLong | JavaLong => ValueType.Long
    case NativeLong | ScalaFloat | JavaFloat => ValueType.Float
    case NativeDouble | ScalaDouble | JavaDouble => ValueType.Double
    case NativeChar | ScalaChar | JavaChar => ValueType.Char
    case NativeBoolean | ScalaBoolean | JavaBoolean => ValueType.Boolean
    case clazz => ClassType(clazz)
  }

  val any: LisaType = AnyType
  val nothing: LisaType = NothingType
  val itself: LisaType = LisaType.of[LisaType]
  val nil: TupleType = TupleType(Nil)
}

trait InstantiatedType { self: LisaType =>
  def runtimeClass: Class[_]
}

trait TestableType {
  def isInstance(obj: Any): Boolean
}

trait CastableType[T] extends TestableType { self: LisaType =>
  def cast(obj: Any): T
}

trait ConstructableType[T] { self: LisaType =>
  def newInstance(args: Seq[Any]): T
}

trait BottomType extends LisaType {
  override def isAssignableTo(other: LisaType): Option[Boolean] = Some(true)
}

object NothingType extends BottomType with ScalaSymbolClassType[Nothing] {
  override def fullName: String = "Nothing"
  override def name: String = fullName

  override def isAssignableFrom(other: LisaType): Option[Boolean] = None

  override def scalaType: universe.Type = universe.typeOf[Nothing]
}

trait AnyType extends LisaType {
  override def isAssignableFrom(other: LisaType): Option[Boolean] = Some(true)
}

object AnyType extends AnyType
  with InstantiatedType with ReferenceType with ValueType with ScalaSymbolClassType[Any] {
  override def name: String = "Any"
  override def fullName: String = "Any"
  override def isAssignableTo(other: LisaType): Option[Boolean] = None

  override def runtimeClass: Class[_] = classOf[Object]

  override def boxedType: ReferenceType = AnyType

  override def scalaType: universe.Type = universe.typeOf[Any]
}

trait ValueType extends LisaType with InstantiatedType {
  def boxedType: ReferenceType
}
object ValueType {
  import moe.roselia.lisa.Reflect.StaticFieldAccessor.jvmBoxedClass
  abstract class ValueTypeBase[T <: AnyVal : universe.TypeTag] extends ValueType with ScalaSymbolClassType[T] {
    override def scalaType: universe.Type = universe.typeOf[T]
    override def boxedType: ReferenceType = ClassType(jvmBoxedClass(classTag.runtimeClass))
    override def runtimeClass: Class[_] = classTag.runtimeClass

    override def isAssignableFrom(other: LisaType): Option[Boolean] =
      boxedType.isAssignableFrom(other).orElse(super.isAssignableFrom(other))

    override def isAssignableTo(other: LisaType): Option[Boolean] =
      super.isAssignableTo(other).orElse(boxedType.isAssignableTo(other))

    override def toString: String = fullName

    override def invoke(args: List[Expression]): Expression =
      throw new UnsupportedOperationException(s"Value type $name is not generic.")
  }

  implicit object Byte extends ValueTypeBase[scala.Byte]
  implicit object Short extends ValueTypeBase[scala.Short]
  implicit object Int extends ValueTypeBase[scala.Int]
  implicit object Long extends ValueTypeBase[scala.Long]
  implicit object Float extends ValueTypeBase[scala.Float]
  implicit object Double extends ValueTypeBase[scala.Double]
  implicit object Boolean extends ValueTypeBase[scala.Boolean]
  implicit object Char extends ValueTypeBase[scala.Char]

  def of[T <: AnyVal](implicit tpe: ValueTypeBase[T]): ValueType = tpe
}

trait ReferenceType extends LisaType {}

trait NullType extends ReferenceType {
  override def isAssignableTo(other: LisaType): Option[Boolean] =
    Some(other.isInstanceOf[ReferenceType])

  override def isAssignableFrom(other: LisaType): Option[Boolean] = Some(other.isInstanceOf[NullType])
}
object NullType extends NullType {
  override def name: String = "Null"

  override def fullName: String = "Null"
}

case class LiteralType(value: Expression, underlyingType: LisaType) extends LisaType with TestableType {
  override def name: String = value.code
  override def fullName: String = s"(Literal $value: ${underlyingType.fullName})"
  override def isAssignableFrom(other: LisaType): Option[Boolean] = other match {
    case LiteralType(`value`, tpe) if tpe <:< underlyingType => Some(true)
    case _ => None
  }
  override def isAssignableTo(other: LisaType): Option[Boolean] = underlyingType.isAssignableTo(other)

  override def isInstance(obj: Any): Boolean = value == obj
}

trait ClassTypeLike[T] extends LisaType
  with InstantiatedType with CastableType[T] with ConstructableType[T] with InvokableProcedure {
  override def name: String = runtimeClass.getSimpleName

  override def fullName: String = runtimeClass.getCanonicalName

  override def isAssignableTo(other: LisaType): Option[Boolean] = other match {
    case c: ClassTypeLike[_] if c.runtimeClass.isAssignableFrom(runtimeClass) => Some(true)
    case _ => None
  }

  override def isAssignableFrom(other: LisaType): Option[Boolean] = other match {
    case c: ClassTypeLike[_] if runtimeClass.isAssignableFrom(c.runtimeClass) => Some(true)
    case _ => None
  }

  override def cast(obj: Any): T = runtimeClass.cast(obj).asInstanceOf[T]
  override def isInstance(obj: Any): Boolean = runtimeClass.isInstance(obj)

  override def newInstance(args: Seq[Any]): T =
    ConstructorCaller.newInstanceForClass(runtimeClass, args).asInstanceOf[T]

  def isGeneric: Boolean = runtimeClass.getTypeParameters.nonEmpty
}

trait ScalaSymbolClassType[T] extends ClassTypeLike[T] {
  import scala.reflect.runtime.{universe => ru}
  def scalaType: ru.Type

  override def name: String = {
    val typeParams = appliedTypeParameters
    if (typeParams.isEmpty) scalaType.typeSymbol.name.decodedName.toString
    else s"(${super.name} ${typeParams.map(_.dealias).map(ScalaSymbolType(_)).map(_.name).mkString(" ")})"
  }
  override def fullName: String = scalaType.toString

  override def runtimeClass: Class[_] = ru.rootMirror.runtimeClass(scalaType)

  private def appliedTypeParameters = {
    scalaType.dealias.typeArgs.filter(_.typeSymbol.isClass)
  }
  private def typeParameters: List[ru.TypeSymbol] = {
    scalaType.dealias.typeParams.filterNot(_.isClass).map(_.asType)
  }

  def typeParameterVariance: List[TypeVariance] =
    typeParameters.map {
      case x if x.isCovariant => TypeVariance.Covariant
      case x if x.isContravariant => TypeVariance.Contravariant
      case _ => TypeVariance.Invariant
    }

  override def isGeneric: Boolean = typeParameters.nonEmpty

  override def isAssignableTo(other: LisaType): Option[Boolean] = other match {
    case symbolClass: ScalaSymbolClassType[_] =>
      Some(scalaType <:< symbolClass.scalaType)
    case _ => super.isAssignableTo(other)
  }

  override def invoke(args: List[Expression]): Expression = {
    if (!isGeneric) throw new UnsupportedOperationException(s"type $fullName is not generic")
    val typeParameter = typeParameters
    if (typeParameter.length != args.length)
      throw new IllegalArgumentException(s"Expected ${typeParameter.length} types but ${args.length} found.")
    val typeArguments = args.map {
      case lt: LisaType => lt
      case ex => throw LisaRuntimeException(ex, new IllegalArgumentException(s"expected a type but ${ex.lisaType} found."))
    }

    val appliedType = ru.appliedType(scalaType, typeArguments.map(_.scalaSymbol))
    appliedType.toLisaType
  }
}

case class ScalaSymbolType[T](scalaType: scala.reflect.runtime.universe.Type)
  extends ScalaSymbolClassType[T] with ReferenceType

case class ClassType[T](override val runtimeClass: Class[T]) extends ScalaSymbolClassType[T] with ReferenceType {
  override lazy val scalaType: universe.Type = {
    val mirror = universe.runtimeMirror(runtimeClass.getClassLoader)
    mirror.classSymbol(runtimeClass).asType.toType
  }
}
object ClassType {
  def of[T : ClassTag]: ClassType[T] = ClassType(classTag.runtimeClass.asInstanceOf[Class[T]])
}

trait TypeLambda extends ReferenceType with InvokableProcedure {
  def construct(typeArgs: Seq[LisaType]): CompoundType
  override def invoke(args: List[Expression]): Expression = construct(args.map {
    case lt: LisaType => lt
    case ex => throw LisaRuntimeException(ex, new IllegalArgumentException(s"an type is expected but ${ex.lisaType.fullName} found."))
  })
}

trait CompoundType extends ReferenceType {
  override def name: String = s"(${constructor.name} ${typeArguments.map(_.name).mkString(" ")})"
  override def fullName: String = s"(${constructor.fullName} ${typeArguments.map(_.fullName).mkString(" ")})"

  def constructor: TypeLambda
  def typeArguments: Seq[LisaType]
  def variances: Seq[TypeVariance]

  override def isAssignableTo(other: LisaType): Option[Boolean] = other match {
    case compoundType: CompoundType if constructor =:= compoundType.constructor =>
      if (variances == compoundType.variances) {
        val argumentsAreAssignable = variances.indices.map { i =>
          val lhs = typeArguments(i)
          val rhs = compoundType.typeArguments(i)
          variances(i) match {
            case TypeVariance.Invariant => lhs =:= rhs
            case TypeVariance.Covariant => lhs <:< rhs
            case TypeVariance.Contravariant => rhs <:< lhs
          }
        }

        if (argumentsAreAssignable.lengthIs == variances.length) Some {
          argumentsAreAssignable.forall(identity)
        } else None
      } else None
    case _ => None
  }

  override def isAssignableFrom(other: LisaType): Option[Boolean] = None
}

trait OverridingType extends LisaType {
  def overrides: Seq[LisaType]

  override def isAssignableFrom(other: LisaType): Option[Boolean] =
    overrides.view.flatMap(_.isAssignableFrom(other)).headOption.map(Function.const(true))
}

trait FunctionType extends TypeLambda {
  def arguments: TupleType
  def returnType: LisaType
  def isVararg: Boolean = false

  override def isAssignableTo(other: LisaType): Option[Boolean] = other match {
    case fnType: FunctionType =>
      if (fnType.arguments <:< arguments && returnType <:< fnType.returnType) {
        Some(true)
      } else None
    case _ => None
  }

  override def isAssignableFrom(other: LisaType): Option[Boolean] = None
}

object SequenceTypeConstructor extends FunctionType {
  override def name: String = "Seq"
  override def fullName: String = "Seq"

  override def arguments: TupleType = TupleType.of(LisaType.itself)

  override def returnType: LisaType = LisaType.itself

  override def construct(typeArgs: Seq[LisaType]): CompoundType = {
    assert(typeArgs.lengthIs == 1)
    SequenceType(typeArgs.head)
  }
}

trait SequenceLikeType extends CompoundType {
  def variances: Seq[TypeVariance] = List(TypeVariance.Covariant)
}
case class SequenceType(elementType: LisaType) extends SequenceLikeType {
  override def constructor: TypeLambda = SequenceTypeConstructor

  override def typeArguments: Seq[LisaType] = Seq(elementType)

  override def isAssignableTo(other: LisaType): Option[Boolean] = other match {
    case SequenceType(el) if elementType <:< el => Some(true)
    case _ => None
  }

  override def isAssignableFrom(other: LisaType): Option[Boolean] = {
    if (other == LisaType.nil) Some(true)
    else None
  }
}

object TupleTypeConstructor extends FunctionType {
  override def name: String = "Tuple"
  override def fullName: String = "Tuple"

  override def construct(typeArgs: Seq[LisaType]): CompoundType =
    TupleType(typeArgs)

  override def isVararg: Boolean = true

  override def arguments: TupleType = TupleType.of(SequenceType(LisaType.itself))

  override def returnType: LisaType = LisaType.itself
}

case class TupleType(types: Seq[LisaType]) extends LisaListLike[LisaType] with CompoundType {
  override def constructor: TypeLambda = TupleTypeConstructor

  override def typeArguments: Seq[LisaType] = types

  override def variances: Seq[TypeVariance] = types.map(Function.const(TypeVariance.Covariant))

  override def list: List[LisaType] = types.toList

  override def toString: String = if (types.isEmpty) "()" else super.toString
}
object TupleType {
  def of(types: LisaType*): TupleType = TupleType(types)
}

trait Untyped extends LisaType {
  override def isAssignableTo(other: LisaType): Option[Boolean] = None
  override def isAssignableFrom(other: LisaType): Option[Boolean] = None
}
object Untyped extends Untyped {
  override def name: String = "untyped"
  override def fullName: String = "untyped"
}

case class TypeVariable(name: String) extends Untyped {
  override def fullName: String = s"untyped.$name"
}
object TypeVariable {
  private def toAlphaVariable(index: Int): String = {
    if (index < 0) toAlphaVariable(-index)
    else if (index >= 0 && index < 26) ('A' + index).toChar.toString
    else toAlphaVariable(index / 26 - 1) + toAlphaVariable(index % 26)
  }
  private val variables = LazyList.from(0).map(toAlphaVariable)

  def generator(prefix: String = ""): Iterator[TypeVariable] = {
    variables.iterator.map(prefix.concat).map(TypeVariable(_))
  }
}

case object FnTypeConstructor extends FunctionType {
  override def name: String = "Fn"
  override def fullName: String = "Fn"
  override def isVararg: Boolean = true
  override def arguments: TupleType = TupleType.of(SequenceType(LisaType.itself))
  override def returnType: LisaType = LisaType.itself

  override def construct(typeArgs: Seq[LisaType]): CompoundType = Fn(TupleType(typeArgs.init), typeArgs.last)
}
case class Fn(arguments: TupleType, returnType: LisaType) extends CompoundType with ScalaSymbolClassType[Any] {
  override def constructor: TypeLambda = FnTypeConstructor
  override def typeArguments: Seq[LisaType] = arguments.types :+ returnType
  override def variances: Seq[TypeVariance] =
    arguments.types.map(_ => TypeVariance.Contravariant) :+ TypeVariance.Covariant

  override lazy val scalaType: universe.Type = {
    import universe._
    assert(arguments.length <= 22)
    val interfaceType = rootMirror.staticClass(s"scala.Function${arguments.length}").toType
    appliedType(interfaceType, typeArguments.map(_.scalaSymbol).toList)
  }
}
