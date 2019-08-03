package moe.roselia.lisa

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.LispExp._

object Preludes {
  lazy val preludeEnvironment: Environment = EmptyEnv.withValues(Seq(
    "+" -> PrimitiveFunction {
      case ls@x::_ if x.isInstanceOf[SInteger] =>
        SInteger(ls.asInstanceOf[List[SInteger]].map(_.value).sum)
      case xs: List[SString] => SString(xs.map(_.value).reduce(_ + _))
    },
    "-" -> PrimitiveFunction {
      case xs: List[SInteger] => SInteger(xs.map(_.value).reduce(_ - _))
    },
    "*" -> PrimitiveFunction {
      case xs: List[SInteger] => SInteger(xs.map(_.value).product)
    },
    "=" -> PrimitiveFunction {
      case lhs::rhs::Nil => SBool(lhs == rhs)
    },
    "print!" -> PrimitiveFunction {
      case x::Nil =>
        print(x)
        NilObj
      case x =>
        print(s"( ${x.mkString(" ")} )")
        NilObj
    },
    "println!" -> PrimitiveFunction {
      case x::Nil =>
        println(x)
        NilObj
      case x =>
        println(s"( ${x.mkString(" ")} )")
        NilObj
    },
    "eval" -> PrimitiveFunction {
      case x::Nil => Evaluator.eval(x, preludeEnvironment).asInstanceOf[Evaluator.EvalSuccess].expression
    }
  ))
}
