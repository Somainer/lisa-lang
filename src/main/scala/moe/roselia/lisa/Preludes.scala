package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Environment, SpecialEnv}
import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Reflect.ScalaBridge.{fromScalaNative, toScalaNative}

import scala.util.Try

object Preludes {
  import javax.script.ScriptEngine
  private val globalJSEngine = new javax.script.ScriptEngineManager().getEngineByName("ecmascript")

  private lazy val primitiveEnvironment: Environment = EmptyEnv.withValues(Seq(
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
      case x::Nil => Evaluator.eval(x, primitiveEnvironment).asInstanceOf[Evaluator.EvalSuccess].expression
    },
    "js" -> PrimitiveFunction {
      case SString(s)::Nil => Reflect.ScalaBridge.fromScalaNative(
        globalJSEngine.eval(s)
      )
    },
    "set-js!" -> PrimitiveFunction {
      case Quote(Symbol(sym))::exp::Nil => {
        globalJSEngine.put(sym, toScalaNative(exp))
        NilObj
      }
      case SString(sym)::exp::Nil => {
        globalJSEngine.put(sym, toScalaNative(exp))
        NilObj
      }
    }
  ))

  private lazy val javaScriptEnv = new SpecialEnv {
    override def has(key: String): Boolean = getValueOption(key).isDefined

    override def getValueOption(key: String): Option[Expression] =
      Try(globalJSEngine.get(key)).filter(x => x != null).map(Reflect.ScalaBridge.fromScalaNative).toOption

  }

  lazy val preludeEnvironment = CombineEnv(Seq(primitiveEnvironment, javaScriptEnv))
}
