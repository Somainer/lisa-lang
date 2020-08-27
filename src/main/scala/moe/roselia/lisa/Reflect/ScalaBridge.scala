package moe.roselia.lisa.Reflect

import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Evaluator.applyToEither

object ScalaBridge {
  private def evalClosure(c: Procedure)(xOrxs: Any) = {
//    println(s"Received $c $xs -> ${xs.map(fromScalaNative).toList}")
//    println(s"-> ${apply(c, xs.map(fromScalaNative).toList)}")
    val xs = ensureSeq(xOrxs)
    applyToEither(c, xs.map(fromScalaNative).toList)
      .fold(ex => throw new RuntimeException(ex), toScalaNative)
  }

  private def ensureSeq(xOrxs: Any) = xOrxs match {
    case x: Seq[Any] => x
    case x => Seq(x)
  }

  def toScalaNative(exp: Expression): Any = exp match {
    case JVMNull => null
    case SBool(b) => b
    case SInteger(i) => if(i.isValidInt) i.toInt else i
    case SString(s) => s
    case WrappedScalaObject(obj) => obj
    case Symbol(sym) => scala.Symbol(sym)
    case SFloat(fl) => fl.toDouble
//    case NilObj => ()
    case SRational(rat) =>
      if(rat.isIntegral) {
        val integral = rat.toIntegral
        if(integral.isValidInt) integral.toInt
        else if (integral.isValidLong) integral.toLong
        else integral
      } else rat.toDouble
    case sNumber: SNumber[_] => sNumber
    case PrimitiveFunction(fn) => (xs: Any) => fn(ensureSeq(xs).map(fromScalaNative).toList)
    case c@Closure(_, _, _, _) =>
      evalClosure(c)(_)
    case r: LisaRecord[_] => r
    case ll: LisaListLike[_] => ll
    case identicalLisaExpression: IdenticalLisaExpression => identicalLisaExpression
  }

  def fromScalaNative(any: Any): Expression = any match {
    case null => JVMNull
    case list: List[_] =>
      LisaList(list.map(fromScalaNative))
    case array: Array[_] => LisaList(array.map(fromScalaNative).toList)
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
    case () => NilObj
    // case ls: List[Any] => WrappedScalaObject(ls)
//    case tuple: Product => LisaList(tuple.productIterator.map(fromScalaNative).toList)
//    case fn: Function[Any, Any] =>
//      PrimitiveFunction(xs => fromScalaNative(fn(xs.map(toScalaNative))))
    case Failure(tp, message) => throw new RuntimeException(s"$tp: $message")
    case otherwise => WrappedScalaObject(otherwise)
  }

  def fromJVMNative(any: Any): Expression = fromScalaNative(javaNativeToScala(any))

  import scala.jdk.CollectionConverters._
  def javaNativeToScala(original: Any) = original match {
    case array: Array[_] => array.toIndexedSeq
    case collection: java.util.Collection[_] => collection.asScala
    case map: java.util.Map[_, _] => map.asScala
    case _ => original
  }

  def jsonLikeToLisa(jsonLike: Any): Expression = jsonLike match {
    case m: Map[String, _] =>
      LisaMapRecord(m.transform { (_, x) => jsonLikeToLisa(x) })
    case l: Seq[_] => LisaList(l.map(jsonLikeToLisa).toList)
    case d: Double =>
      if (d.isValidInt) SInteger(d.toInt) else SFloat(d)
    case o => fromScalaNative(o)
  }
}
