package moe.roselia.lisa.Reflect

import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Evaluator.apply

object ScalaBridge {
  private def evalClosure(c: Closure)(xOrxs: Any) = {
//    println(s"Received $c $xs -> ${xs.map(fromScalaNative).toList}")
//    println(s"-> ${apply(c, xs.map(fromScalaNative).toList)}")
    val xs = ensureSeq(xOrxs)
    apply(c, xs.map(fromScalaNative).toList)
      .fold(ex => Failure("Scala Bridge Eval Error", ex), toScalaNative)
  }

  private def ensureSeq(xOrxs: Any) = xOrxs match {
    case x: Seq[Any] => x
    case x => Seq(x)
  }

  def toScalaNative(exp: Expression): Any = exp match {
    case SBool(b) => b
    case SInteger(i) => i
    case SString(s) => s
    case WrappedScalaObject(obj) => obj
    case Symbol(sym) => scala.Symbol(sym)
    case SFloat(fl) => fl
    case NilObj => ()
    case PrimitiveFunction(fn) => (xs: Any) => fn(ensureSeq(xs).map(fromScalaNative).toList)
    case c@Closure(_, _, _, _) =>
      evalClosure(c)(_)
  }

  def fromScalaNative(any: Any): Expression = any match {
    case ex: Expression => ex
    case b: Boolean => SBool(b)
    case i: Int => SInteger(i)
    case i: java.lang.Integer => SInteger(i.intValue())
    case d: Double => SFloat(d)
    case f: Float => SFloat(f)
    case s: String => SString(s)
    case scala.Symbol(sym) => Symbol(sym)
    case () => NilObj
    case ls: List[Any] => WrappedScalaObject(ls)
//    case fn: Function[Any, Any] =>
//      PrimitiveFunction(xs => fromScalaNative(fn(xs.map(toScalaNative))))
    case Failure(tp, message) => throw new RuntimeException(s"$tp: $message")
    case otherwise => WrappedScalaObject(otherwise)
  }
}
