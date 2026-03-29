package moe.roselia.lisa.Reflect

import moe.roselia.lisa.LispExp.*
import moe.roselia.lisa.Evaluator.applyToEither

object ScalaBridge:
  private[lisa] def evalClosure(c: Procedure)(xOrxs: Any): Any =
    val xs = ensureSeq(xOrxs)
    applyToEither(c, xs.map(fromScalaNative).toList)
      .fold(ex => throw new RuntimeException(ex), toScalaNative)

  private def ensureSeq(xOrxs: Any): Seq[Any] = xOrxs match
    case x: Seq[Any @unchecked] => x
    case x => Seq(x)

  def toScalaNative(exp: Expression): Any = exp match
    case JVMNull => null
    case SBool(b) => b
    case SInteger(i) => if i.isValidInt then i.toInt else i
    case SString(s) => s
    case WrappedScalaObject(obj) => obj
    case Symbol(sym) => scala.Symbol(sym)
    case SFloat(fl) => fl.toDouble
    case SRational(rat) =>
      if rat.isIntegral then
        val integral = rat.toIntegral
        if integral.isValidInt then integral.toInt
        else if integral.isValidLong then integral.toLong
        else integral
      else rat.toDouble
    case sNumber: SNumber[?] => sNumber
    case x => x

  def fromScalaNative(any: Any): Expression = any match
    case null => JVMNull
    case list: List[?] =>
      LisaList(list.map(fromScalaNative))
    case array: Array[?] => LisaList(array.map(fromScalaNative).toList)
    case ex: Expression => ex
    case b: Boolean => SBool(b)
    case s: Short => SInteger(s.intValue())
    case b: Byte => SInteger(b.intValue())
    case c: Char => SString(c.toString)
    case i: Int => SInteger(i)
    case l: Long => SInteger(LisaInteger(l))
    case l: java.lang.Long => SInteger(LisaInteger(l))
    case i: java.lang.Integer => SInteger(i.intValue())
    case d: Double => SFloat(d)
    case f: Float => SFloat(f.toDouble)
    case s: String => SString(s)
    case bi: LisaInteger => SInteger(bi)
    case di: LisaDecimal => SFloat(di)
    case scala.Symbol(sym) => Symbol(sym)
    case _: Unit => NilObj
    case Failure(tp, message) => throw new RuntimeException(s"$tp: $message")
    case otherwise => WrappedScalaObject(otherwise)

  def fromJVMNative(any: Any): Expression = fromScalaNative(javaNativeToScala(any))

  import scala.jdk.CollectionConverters.*
  def javaNativeToScala(original: Any): Any = original match
    case array: Array[?] => array.toIndexedSeq
    case collection: java.util.Collection[?] => collection.asScala
    case map: java.util.Map[?, ?] => map.asScala
    case _ => original

  def jsonLikeToLisa(jsonLike: Any): Expression = jsonLike match
    case m: Map[String @unchecked, ?] =>
      LisaMapRecord(m.transform { (_, x) => jsonLikeToLisa(x) })
    case l: Seq[?] => LisaList(l.map(jsonLikeToLisa).toList)
    case d: Double =>
      if d.isValidInt then SInteger(d.toInt) else SFloat(d)
    case o => fromScalaNative(o)
end ScalaBridge
