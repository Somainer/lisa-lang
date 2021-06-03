package moe.lisa.core.expression

import com.somainer.nameof.NameOf._

import moe.lisa.lang.Atom

class Constant(val value: Any, tag: Constant.TypeTag) {
  import Constant._

  def booleanValue: Boolean =
    if tag == BooleanTag then throw new Error(s"$value is not ${nameOfType[Boolean]}")
    value.asInstanceOf[Boolean]

  def byteValue: Byte = tag match {
    case ByteTag   => value.asInstanceOf[Byte]
    case ShortTag  => value.asInstanceOf[Short].toByte
    case CharTag   => value.asInstanceOf[Char].toByte
    case IntTag    => value.asInstanceOf[Int].toByte
    case LongTag   => value.asInstanceOf[Long].toByte
    case FloatTag  => value.asInstanceOf[Float].toByte
    case DoubleTag => value.asInstanceOf[Double].toByte
    case _         => throw new Error("value " + value + " is not a Byte")
  }

  def shortValue: Short = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toShort
    case ShortTag  => value.asInstanceOf[Short]
    case CharTag   => value.asInstanceOf[Char].toShort
    case IntTag    => value.asInstanceOf[Int].toShort
    case LongTag   => value.asInstanceOf[Long].toShort
    case FloatTag  => value.asInstanceOf[Float].toShort
    case DoubleTag => value.asInstanceOf[Double].toShort
    case _         => throw new Error("value " + value + " is not a Short")
  }

  def charValue: Char = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toChar
    case ShortTag  => value.asInstanceOf[Short].toChar
    case CharTag   => value.asInstanceOf[Char]
    case IntTag    => value.asInstanceOf[Int].toChar
    case LongTag   => value.asInstanceOf[Long].toChar
    case FloatTag  => value.asInstanceOf[Float].toChar
    case DoubleTag => value.asInstanceOf[Double].toChar
    case _         => throw new Error("value " + value + " is not a Char")
  }

  def intValue: Int = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toInt
    case ShortTag  => value.asInstanceOf[Short].toInt
    case CharTag   => value.asInstanceOf[Char].toInt
    case IntTag    => value.asInstanceOf[Int]
    case LongTag   => value.asInstanceOf[Long].toInt
    case FloatTag  => value.asInstanceOf[Float].toInt
    case DoubleTag => value.asInstanceOf[Double].toInt
    case _         => throw new Error("value " + value + " is not an Int")
  }

  def longValue: Long = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toLong
    case ShortTag  => value.asInstanceOf[Short].toLong
    case CharTag   => value.asInstanceOf[Char].toLong
    case IntTag    => value.asInstanceOf[Int].toLong
    case LongTag   => value.asInstanceOf[Long]
    case FloatTag  => value.asInstanceOf[Float].toLong
    case DoubleTag => value.asInstanceOf[Double].toLong
    case _         => throw new Error("value " + value + " is not a Long")
  }

  def floatValue: Float = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toFloat
    case ShortTag  => value.asInstanceOf[Short].toFloat
    case CharTag   => value.asInstanceOf[Char].toFloat
    case IntTag    => value.asInstanceOf[Int].toFloat
    case LongTag   => value.asInstanceOf[Long].toFloat
    case FloatTag  => value.asInstanceOf[Float]
    case DoubleTag => value.asInstanceOf[Double].toFloat
    case _         => throw new Error("value " + value + " is not a Float")
  }

  def doubleValue: Double = tag match {
    case ByteTag   => value.asInstanceOf[Byte].toDouble
    case ShortTag  => value.asInstanceOf[Short].toDouble
    case CharTag   => value.asInstanceOf[Char].toDouble
    case IntTag    => value.asInstanceOf[Int].toDouble
    case LongTag   => value.asInstanceOf[Long].toDouble
    case FloatTag  => value.asInstanceOf[Float].toDouble
    case DoubleTag => value.asInstanceOf[Double]
    case _         => throw new Error("value " + value + " is not a Double")
  }

  def bigIntValue: BigInt = tag match
    case BigIntTag => value.asInstanceOf[BigInt]
    case BigDecimalTag => bigDecimalValue.toBigInt
    case _ if isLongRange => BigInt(longValue)
    case _ => throw new Error(s"value $value is not a ${nameOf(BigInt)}")

  def bigDecimalValue: BigDecimal = tag match
    case BigDecimalTag => value.asInstanceOf[BigDecimal]
    case BigIntTag => BigDecimal(bigIntValue)
    case _ if isNumeric => BigDecimal(doubleValue)
    case _ => throw new Error(s"value $value is not a ${nameOf(BigDecimal)}")

  def stringValue: String = value.toString

  def atomValue: Atom = tag match
    case AtomTag => value.asInstanceOf[Atom]
    case _  => throw new Error(s"value $value is not an ${nameOfType[Atom]}")

  def isBigInt: Boolean = tag == BigIntTag
  def isBigDecimal: Boolean = tag == BigDecimalTag
  def isNull: Boolean = tag == NullTag
  def isByteRange: Boolean     = isIntRange && Byte.MinValue <= intValue && intValue <= Byte.MaxValue
  def isShortRange: Boolean    = isIntRange && Short.MinValue <= intValue && intValue <= Short.MaxValue
  def isCharRange: Boolean     = isIntRange && Char.MinValue <= intValue && intValue <= Char.MaxValue
  def isIntRange: Boolean      = ByteTag <= tag && tag <= IntTag
  def isLongRange: Boolean     = ByteTag <= tag && tag <= LongTag
  def isFloatRange: Boolean    = ByteTag <= tag && tag <= FloatTag
  def isNumeric: Boolean       = ByteTag <= tag && tag <= DoubleTag
  def isNonUnitAnyVal: Boolean = BooleanTag <= tag && tag <= DoubleTag
  def isAnyVal: Boolean        = UnitTag <= tag && tag <= DoubleTag

  override def toString: String = s"Constant($value)"
}

object Constant {
  opaque type TypeTag = Int

  final val NoTag: TypeTag = 0
  final val UnitTag: TypeTag = 1
  final val BooleanTag: TypeTag = 2
  final val ByteTag: TypeTag = 3
  final val ShortTag: TypeTag = 4
  final val CharTag: TypeTag = 5
  final val IntTag: TypeTag = 6
  final val LongTag: TypeTag = 7
  final val BigIntTag: TypeTag = 8
  final val FloatTag: TypeTag = 9
  final val DoubleTag: TypeTag = 10
  final val BigDecimalTag: TypeTag = 11
  final val StringTag: TypeTag = 12
  final val AtomTag: TypeTag = 13
  final val NullTag: TypeTag = 14
  final val ClassTag: TypeTag = 15

  extension(tt: TypeTag)
    def <=(that: TypeTag) = tt.asInstanceOf[Int] <= that

  def apply(value: Any): Constant = value match
    case Nil => new Constant(value, UnitTag)
    case _: Boolean => new Constant(value, BooleanTag)
    case _: Byte => new Constant(value, ByteTag)
    case _: Short => new Constant(value, ShortTag)
    case _: Char => new Constant(value, CharTag)
    case _: Int => new Constant(value, IntTag)
    case _: Long => new Constant(value, LongTag)
    case _: BigInt => new Constant(value, BigIntTag)
    case _: Float => new Constant(value, FloatTag)
    case _: Double => new Constant(value, DoubleTag)
    case _: BigDecimal => new Constant(value, BigDecimalTag)
    case _: String => new Constant(value, StringTag)
    case _: Atom => new Constant(value, AtomTag)
    case null => new Constant(value, NullTag)
    case _: Class[?] => new Constant(value, ClassTag)
}
