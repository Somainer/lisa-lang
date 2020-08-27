package moe.roselia.lisa

import scala.annotation.tailrec
import languageFeature.implicitConversions
import Integral.Implicits._

case class Rational[T: Integral] private (numerator: T, denominator: T) extends Ordered[Rational[T]] {
  private[this] def evidence: Integral[T] = implicitly
  def toDouble = numerator.toDouble / denominator.toDouble
  def flatMap(f: (T, T) => Rational[T]) = f(numerator, denominator)
  def map(f: (T, T) => (T, T)) =
    flatMap { (x, y) =>
        val (a, b) = f(x, y)
        Rational(a, b)
    }

  def map2(that: Rational[T])(f: (T, T, T, T) => (T, T)) =
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
    if(isIntegral) denominator.toString else s"$numerator/$denominator"
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

  def apply[T: Integral](numerator: T): Rational[T] = apply(numerator, implicitly[Integral[T]].one)

  @tailrec
  def gcd[T: Integral](a: T, b: T): T =
    if (b == 0) a else gcd(b, a % b)

  def lcm[T: Integral](a: T, b: T): T = a / gcd(a, b) * b

  def fromDouble[T](d: Double)(implicit integral: Integral[T]): Rational[T] = {
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

  trait RationalIsNumeric[T] extends Numeric[Rational[T]] with Fractional[Rational[T]] {
    protected[this] implicit def integralEvidence: Integral[T]

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

  def RationalIsNumericEvidenceMaker[T: Integral]: RationalIsNumeric[T] = new RationalIsNumeric[T] {
    override def integralEvidence: Integral[T] = implicitly
  }

  implicit val RationalOfIntIsNumeric: RationalIsNumeric[Int] = RationalIsNumericEvidenceMaker
  implicit val RationalOfBigIntIsNumeric: RationalIsNumeric[BigInt] = RationalIsNumericEvidenceMaker

  trait Implicits {
    implicit def integralToSRational[T](t: T)(implicit isIntegral: Integral[T]): Rational[T] =
      Rational(t, isIntegral.one)

    implicit def sRationalToDouble(r: Rational[_]): Double = r.toDouble
  }
  object Implicits extends Implicits
  import Implicits._
}