package moe.roselia.lisa.Reflect

import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Evaluator.apply

object ScalaBridge {
  private def evalClosure(c: Closure)(xs: Any*) = {
    apply(c, xs.map(fromScalaNative).toList).map(toScalaNative).getOrElse(())
  }

  def toScalaNative(exp: Expression): Any = exp match {
    case SBool(b) => b
    case SInteger(i) => i
    case SString(s) => s
    case WrappedScalaObject(obj) => obj
    case Symbol(sym) => scala.Symbol(sym)
    case SFloat(fl) => fl
    case NilObj => ()
    case PrimitiveFunction(fn) => (xs: List[Any]) => fn(xs.map(fromScalaNative))
    case c@Closure(_, _, _, _) =>
      evalClosure(c)(_)
  }

  def fromScalaNative(any: Any): Expression = any match {
    case b: Boolean => SBool(b)
    case i: Int => SInteger(i)
    case d: Double => SFloat(d)
    case f: Float => SFloat(f)
    case s: String => SString(s)
    case scala.Symbol(sym) => Symbol(sym)
    case () => NilObj
    case fn: (List[Any] => Any) =>
      PrimitiveFunction(xs => fromScalaNative(fn(xs.map(toScalaNative))))
    case otherwise => WrappedScalaObject(otherwise)
  }
}
