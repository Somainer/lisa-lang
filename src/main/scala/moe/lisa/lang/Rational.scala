package moe.lisa.lang

import scala.annotation.tailrec
import scala.language.implicitConversions
import Integral.Implicits._

case class Rational[T] private (numerator: T, denominator: T)(using Integral[T]) extends Number, Ordered[Rational[T]] {
  private inline def evidence: Integral[T] = summon
  def toDouble = numerator.toDouble / denominator.toDouble
  inline def flatMap(inline f: (T, T) => Rational[T]) = f(numerator, denominator)
  inline def map(inline f: (T, T) => (T, T)) =
    flatMap { (x, y) =>
        val (a, b) = f(x, y)
        Rational(a, b)
    }

  inline def map2(that: Rational[T])(inline f: (T, T, T, T) => (T, T)) =
    flatMap((x, y) => that.map(f(x, y, _, _)))

  def reciprocal = map { (a, b) => (b, a) }

  def +(that: Rational[T]) = map2(that) {
    (x, y, a, b) => (x * b + a * y, y * b)
  }
  def -(that: Rational[T]) = map2(that) {
    (x, y, a, b) => (x * b - a * y, y * b)
  }
  def *(that: Rational[T]) = map2(that) {
    (x, y, a, b) => (x * a, y * b)
  }
  def /(that: Rational[T]) = this * that.reciprocal
  def abs = map {
    (a, b) => (a.abs, b.abs)
  }
  def unary_- = map {
    (a, b) => (-a, b)
  }

  def sign = (numerator.sign * denominator.sign).sign

  override def compare(that: Rational[T]): Int =
    (this - that).sign.toInt

  def toIntegral = numerator / denominator

  def isIntegral = {
    evidence.equiv(denominator, evidence.one)
  }

  override def toString: String =
    if isIntegral then denominator.toString else s"$numerator/$denominator"

  def intValue(): Int = toIntegral.toInt
  def longValue(): Long = toIntegral.toLong
  def floatValue(): Float = toDouble.toFloat
  def doubleValue(): Double = toDouble
}

object Rational {
  def apply[T: Integral](numerator: T, denominator: T): Rational[T] = {
    require(denominator != 0, "Denominator can not be 0.")
    val g = gcd(numerator, denominator)
    val na = numerator / g
    val nb = denominator / g
    if (nb.sign.toInt > 0) new Rational(na, nb)
    else new Rational(-na, -nb)
  }

  def apply[T: Integral](numerator: T): Rational[T] = apply(numerator, summon[Integral[T]].one)

  @tailrec
  def gcd[T: Integral](a: T, b: T): T =
    if (b == 0) a else gcd(b, a % b)

  def lcm[T: Integral](a: T, b: T): T = a / gcd(a, b) * b

  def fromDouble[T](d: Double)(using integral: Integral[T]): Rational[T] = {
    var num = d
    val base = 10
    val integralBase = integral.fromInt(base)
    var denominator = integral.one
    while (num != Math.floor(num)) {
      num *= base
      denominator *= integralBase
    }
    Rational[T](integral.fromInt(num.toInt), integralBase)
  }

  trait RationalIsNumeric[T](using integralEvidence: Integral[T]) extends Numeric[Rational[T]], Fractional[Rational[T]] {
    override def plus(x: Rational[T], y: Rational[T]): Rational[T] = x + y
    override def minus(x: Rational[T], y: Rational[T]): Rational[T] = x - y
    override def times(x: Rational[T], y: Rational[T]): Rational[T] = x * y
    override def div(x: Rational[T], y: Rational[T]): Rational[T] = x / y
    override def negate(x: Rational[T]): Rational[T] = -x

    override def fromInt(x: Int): Rational[T] = {
      Rational(integralEvidence.fromInt(x), integralEvidence.one)
    }

    override def toInt(x: Rational[T]): Int = integralEvidence.toInt(x.toIntegral)
    override def toLong(x: Rational[T]): Long = integralEvidence.toLong(x.toIntegral)
    override def toFloat(x: Rational[T]): Float = toDouble(x).toFloat
    override def toDouble(x: Rational[T]): Double = x.toDouble

    override def compare(x: Rational[T], y: Rational[T]): Int = x.compare(y)

    override def parseString(str: String): Option[Rational[T]] = str match {
      case s"$n/$d" => for {
        ni <- integralEvidence.parseString(n)
        di <- integralEvidence.parseString(d)
      } yield Rational(ni, di)
      case s"$i" => integralEvidence.parseString(i).map(Rational(_, integralEvidence.one))
      case _ => None
    }
  }

  given RationalOfIntIsNumeric: RationalIsNumeric[Int] with {}
  given RationalOfBigIntIsNumeric: RationalIsNumeric[BigInt] with {}
}
