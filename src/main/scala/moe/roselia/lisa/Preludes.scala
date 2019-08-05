package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Environment, SpecialEnv}
import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Reflect.PackageAccessor
import moe.roselia.lisa.Reflect.ScalaBridge.{fromScalaNative, toScalaNative}

import scala.util.Try

object Preludes {
  type EngineType = javax.script.ScriptEngine with javax.script.Invocable
  private lazy val globalJSEngine = new javax.script.ScriptEngineManager()
    .getEngineByName("ecmascript").asInstanceOf[EngineType]
  private lazy val globalScalaEngine = {
    val engine = new javax.script.ScriptEngineManager()
      .getEngineByName("scala").asInstanceOf[EngineType]
    engine
  }

  private lazy val selectablePreludes = Map(
    "javascript" -> javaScriptPlugin,
    "javascript-cross" -> javaScriptEnv,
    "scala" -> scalaPlugin,
    "scala-cross" -> scalaEnv,
    "scala-root" -> PackageAccessor.rootScalaEnv
  )

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
        print(s"${x.mkString(" ")}")
        NilObj
    },
    "println!" -> PrimitiveFunction {
      case x::Nil =>
        println(x)
        NilObj
      case x =>
        println(s"${x.mkString(" ")}")
        NilObj
    },
    "input" -> PrimitiveFunction {
      x =>
        SString(scala.io.StdIn.readLine(x.mkString(" ")))
    },
    "int" -> PrimitiveFunction {
      case v::Nil => SInteger {
        v match {
          case SString(x) => x.toInt
          case SInteger(x) => x
          case SFloat(f) => f.toInt
          case SBool(b) => if(b) 1 else 0
        }
      }
    },
    "eval" -> PrimitiveFunction {
      case x::Nil => Evaluator.eval(x, primitiveEnvironment).asInstanceOf[Evaluator.EvalSuccess].expression
    },
    "truthy?" -> PrimitiveFunction {
      case ex::Nil => LispExp.SBool {
        ex match {
          case LispExp.NilObj => false
          case LispExp.SInteger(n) => n != 0
          case LispExp.SFloat(n) => n != 0.0
          case LispExp.SBool(b) => b
          case LispExp.SString(s) => s.nonEmpty
          case _ => true
        }
      }
      case _ => LispExp.Failure("Runtime Error", "truthy? can only apply to one value")
    },
    "import-env!" -> SideEffectFunction {
      case (Quote(Symbol(sym))::Nil, env) =>
        if (selectablePreludes.contains(sym))
          (NilObj, CombineEnv(Seq(env, selectablePreludes(sym))))
        else (Failure("Import Error", s"Environment $sym not found"), env)
      case s => (Failure("Import Error", s"Cannot import ${s._1}"), s._2)
    },
    "list" -> PrimitiveFunction (xs => WrappedScalaObject(xs.map(toScalaNative))),
    "seq" -> PrimitiveFunction (xs => WrappedScalaObject(xs.map(toScalaNative).toIndexedSeq))
  ))

  private lazy val javaScriptEnv = new SpecialEnv {
    override def has(key: String): Boolean = getValueOption(key).isDefined

    override def getValueOption(key: String): Option[Expression] =
      Try(globalJSEngine.get(key)).filter(x => x != null).map(Reflect.ScalaBridge.fromScalaNative).toOption

  }

  private lazy val (scalaPlugin, scalaEnv) = makeEnvironment(globalScalaEngine, "scala")

  private lazy val javaScriptPlugin = EmptyEnv.withValues(Seq(
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
    },
    "get-js" -> PrimitiveFunction {
      case Quote(Symbol(sym))::Nil =>
        javaScriptEnv.getValueOption(sym).getOrElse(Failure("Lookup Error", s"Value $sym not found in JS env."))
      case Quote(Symbol(sym))::alt::Nil =>
        javaScriptEnv.getValueOption(sym).getOrElse(alt)
      case _ => Failure("Argument Error", "")
    }
  ))

  private def makeEnvironment(engine: => EngineType, prefix: String) = {
    if (engine == null) throw new NullPointerException("Fuck NPE!")
    val environment = new SpecialEnv {
      override def getValueOption(key: String): Option[Expression] =
        Try(engine.getBindings(javax.script.ScriptContext.ENGINE_SCOPE).get(key)).filter(x => x != null).map(Reflect.ScalaBridge.fromScalaNative).toOption
    }
    (EmptyEnv.withValues(Seq(
      prefix -> PrimitiveFunction {
        case SString(s)::Nil => Reflect.ScalaBridge.fromScalaNative(
          engine.eval(s)
        )
      },
      s"set-$prefix!" -> PrimitiveFunction {
        case Quote(Symbol(sym))::exp::Nil => {
          engine.put(sym, toScalaNative(exp))
          NilObj
        }
        case SString(sym)::exp::Nil => {
          engine.put(sym, toScalaNative(exp))
          NilObj
        }
      },
      s"get-$prefix" -> PrimitiveFunction {
        case Quote(Symbol(sym))::Nil =>
          environment.getValueOption(sym).getOrElse(Failure("Lookup Error", s"Value $sym not found in $prefix env."))
        case Quote(Symbol(sym))::alt::Nil =>
          environment.getValueOption(sym).getOrElse(alt)
        case _ => Failure("Argument Error", "")
      }

    )), environment)
  }


  lazy val preludeEnvironment = CombineEnv(Seq(primitiveEnvironment))
}
