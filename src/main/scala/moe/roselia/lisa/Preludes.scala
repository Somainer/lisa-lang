package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Environment, MutableEnv, NameSpacedEnv, SpecialEnv, TransparentLayer}
import moe.roselia.lisa.Evaluator.{EvalFailure, EvalResult, EvalSuccess}
import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Reflect.{PackageAccessor, ToolboxDotAccessor}
import moe.roselia.lisa.Reflect.ScalaBridge.{fromScalaNative, toScalaNative}

import scala.collection.immutable.Vector
import scala.util.Try

object Preludes extends LispExp.Implicits {
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
    "scala-root" -> PackageAccessor.rootScalaEnv,
    "dot-accessor" -> ToolboxDotAccessor.accessEnv
  )

  private lazy val primitiveEnvironment: Environment = EmptyEnv.withValues(Seq(
    "+" -> PrimitiveFunction {
      case ls@x::_ if x.isInstanceOf[SInteger] =>
        SInteger(ls.asInstanceOf[List[SInteger]].map(_.value).sum)
      case xs: List[SString] => SString(xs.map(_.value).reduce(_ + _))
    },
    "-" -> PrimitiveFunction {
      case SInteger(x)::Nil => -x
      case xs: List[SInteger] => SInteger(xs.map(_.value).reduce(_ - _))
    },
    "*" -> PrimitiveFunction {
      case xs: List[SInteger] => SInteger(xs.map(_.value).product)
    },
    "=" -> PrimitiveFunction {
      case lhs::rhs::Nil => lhs == rhs
    }.withArity(2),
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
      case x =>
        println(s"${x.mkString(" ")}")
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
    }.withArity(1),
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
    }.withArity(1),
    "import-env!" -> PrimitiveMacro {
      case (Symbol(sym)::Nil, env) =>
        if (selectablePreludes.contains(sym))
          (NilObj, CombineEnv(Seq(env, selectablePreludes(sym))))
        else (Failure("Import Error", s"Environment $sym not found"), env)
      case (Symbol(sym)::Symbol(ns)::Nil, env) =>
        if (selectablePreludes.contains(sym))
          (NilObj, CombineEnv(Seq(env, NameSpacedEnv(ns, selectablePreludes(sym)))))
        else (Failure("Import Error", s"Environment $sym not found"), env)
      case s => (Failure("Import Error", s"Cannot import ${s._1}"), s._2)
    },
    "list" -> PrimitiveFunction (xs => WrappedScalaObject(xs)),
    "seq" -> PrimitiveFunction (xs => WrappedScalaObject(xs.toIndexedSeq)),
    "wrap-scala" -> PrimitiveFunction {
      x => WrappedScalaObject(toScalaNative(x(0)))
    }.withArity(1),
    "wrap" -> PrimitiveFunction { x => WrappedScalaObject(x(0)) }.withArity(1),
    "map" -> PrimitiveFunction {
      case WrappedScalaObject(ls: Iterable[Any])::fn::Nil => fn match {
        case c: Closure =>
          val newList = ls.map(x => Evaluator.apply(c, List(fromScalaNative(x))))
          if(newList forall (_.isRight))
            WrappedScalaObject(newList.map(_.toOption.get))
          else newList.find(_.isLeft).get.left.toOption.map(Failure("map Error", _)).get
        case PrimitiveFunction(fn) => WrappedScalaObject {
          ls.map(x => fn(List(fromScalaNative(x))))
        }
        case WrappedScalaObject(obj) =>
          WrappedScalaObject(ls.map(x =>
            obj.asInstanceOf[{def apply(a: Any): Any}].apply(x)))
        case _ => Failure("map Error", s"Cannot map $fn on $ls")
      }
      case _ => Failure("Arity Error", "map only accepts 2 arguments, a seq-like and a function-like.")
    }.withArity(2),
    "filter" -> PrimitiveFunction {
      case WrappedScalaObject(ls: Iterable[Any])::fn::Nil => {
        def ensureBool(e: Expression) = e match {
          case SBool(b) => b
          case _ => throw new IllegalArgumentException("Function should return a Bool.")
        }
        fn match {
          case c: Closure =>
            val newList = ls.map(x => Evaluator.apply(c, List(fromScalaNative(x))))
            if(newList forall (_.isRight))
              WrappedScalaObject(newList.map(_.toOption.get).zip(ls).filter(x => ensureBool(x._1)).map(_._2))
            else newList.find(_.isLeft).get.left.toOption.map(Failure("filter Error", _)).get
          case PrimitiveFunction(fn) => WrappedScalaObject {
            ls.filter(x => ensureBool(fn(List(fromScalaNative(x)))))
          }
          case WrappedScalaObject(obj) =>
            WrappedScalaObject(ls.filter(x =>
              obj.asInstanceOf[{def apply(a: Any): Boolean}].apply(x)))
          case _ => Failure("filter Error", s"Cannot filter $fn on $ls")
        }
      }
      case _ => Failure("Arity Error", "filter only accepts 2 arguments, a seq-like and a function-like.")
    }.withArity(2),
    "iter" -> PrimitiveFunction {
      case x::Nil => x match {
        case SString(s) => WrappedScalaObject(s.split("").toIndexedSeq)
        case WrappedScalaObject(xs: Iterable[Any]) => WrappedScalaObject(xs)
        case _ => Failure("iter Error", s"$x is not iterable.")
      }
      case _ => Failure("Arity Error", "iter only accepts 1 argument, an iterable.")
    }.withArity(1),
    "length" -> PrimitiveFunction {
      case arg::Nil => arg match {
        case mha: MayHaveArity if mha.arity.isDefined => mha.arity.get
        case SString(s) => s.length
        case WrappedScalaObject(ls: Seq[Any]) => ls.length
        case WrappedScalaObject(other) => ToolboxDotAccessor.accessDot("length")(other).asInstanceOf[Int]
        case other => Failure("Runtime Error", s"Can not get length for $other.")
      }
      case other => Failure("Arity Error", s"length only accepts one argument but ${other.length} found.")
    }.withArity(1),
    "set!" -> PrimitiveMacro {
      case (Symbol(x)::va::Nil, e) =>
        if (e.isMutable(x))
          Evaluator.eval(va, e) match {
            case EvalSuccess(expression, _) => (NilObj, e.forceUpdated(x, expression))
            case EvalFailure(message) => (Failure("Eval Failure", message), e)
          }
        else (Failure("set! Error", s"Can not assign an immutable value $x."), e)
      case (other, e) => (Failure("Arity Error", s"set! only accepts a symbol and an expression, but $other found."), e)
    }.withArity(2),
    "define-mutable!" -> PrimitiveMacro {
      case (Symbol(x)::Nil, e) =>
        (NilObj, TransparentLayer(MutableEnv.createEmpty.addValue(x, e.getValueOption(x).getOrElse(NilObj)), e))
      case (_, e) => (Failure("Define Failure", "define-mutable! only accepts one symbol."), e)
    }.withArity(1),
    "group!" -> PrimitiveMacro {
      case (xs, e) => xs.foldLeft[EvalResult](EvalSuccess(NilObj, e)) {
        case (EvalSuccess(_, env), x) => Evaluator.eval(x, env)
        case (f, _) => f
      } match {
        case EvalSuccess(expression, env) => expression -> env
        case EvalFailure(msg) => Failure("Group Code Execution Failure", msg) -> e
      }
    },
    "block" -> PrimitiveMacro {
      case (xs, e) => xs.foldLeft[EvalResult](EvalSuccess(NilObj, e.newFrame)) {
        case (EvalSuccess(_, env), x) => Evaluator.eval(x, env)
        case (f, _) => f
      } match {
        case EvalSuccess(expression, _) => expression -> e
        case EvalFailure(msg) => Failure("Block Code Execution Failure", msg) -> e
      }
    },
    "while" -> PrimitiveMacro {
      case (predicate::body::Nil, e) =>
        @annotation.tailrec
        def repeat(env: Environment): EvalResult = Evaluator.eval(predicate, env) match {
          case EvalSuccess(SBool(true), _) =>
            Evaluator.eval(body, env) match {
              case EvalSuccess(_, ne) => repeat(ne)
              case f => f
            }
          case f@EvalSuccess(SBool(false), _) => f
          case EvalSuccess(expression, _) => EvalFailure(s"While predicate $expression is not a Bool.")
          case f => f
        }
        repeat(e) match {
          case EvalSuccess(_, en) => (NilObj, en)
          case EvalFailure(msg) => Failure("While execution failure", msg) -> e
        }
    }.withArity(2),
    "help" -> PrimitiveFunction {
      case exp::Nil =>
        val docString = exp.docString
        if(docString.nonEmpty) docString else exp.code
      case _ => Failure("Help error", "You can only get help from one object.")
    }.withDocString("help :: Any => String\nGet document string from a object.").withArity(1),
    "panic!" -> PrimitiveFunction {
      exp => throw new RuntimeException(exp.mkString(" "))
    },
    "apply" -> PrimitiveFunction {
      case fn::WrappedScalaObject(s: Seq[Expression])::Nil =>
        Evaluator.apply(fn, s.toList).fold(Failure("Apply Failure", _), x => x)
      case _ => Failure("Apply Failure", "Apply expects 2 arguments.")
    }.withArity(2),
    "limit-arity" -> PrimitiveFunction {
      case SInteger(n)::fn::Nil =>
        val argList = 0.until(n).map(i => s"arg$i").map(Symbol).toList
        Closure(argList, Apply(fn, argList), EmptyEnv).copyDocString(fn)
    }.withArity(2).withDocString("Limit va-arg function to accept n arguments"),
    "get-doc" -> PrimitiveFunction {
      case f1::Nil => f1.docString
    }.withArity(1),
    "set-doc" -> PrimitiveFunction {
      case f1::SString(s)::Nil => f1.withDocString(s)
    }.withArity(2)
  ))

  private lazy val (scalaPlugin, scalaEnv) = makeEnvironment(globalScalaEngine, "scala")

  private lazy val (javaScriptPlugin, javaScriptEnv) = makeEnvironment(globalJSEngine, "js")

  private def makeEnvironment(engine: => EngineType, prefix: String) = {
    if (engine == null) throw new NullPointerException("The engine is not found")
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
      s"set-$prefix!" -> PrimitiveMacro {
        case (Symbol(sym)::exp::Nil, e) =>
          engine.put(sym, toScalaNative(exp))
          (NilObj, e)
        case (SString(sym)::exp::Nil, e) =>
          engine.put(sym, toScalaNative(exp))
          NilObj -> e
      },
      s"get-$prefix" -> PrimitiveMacro {
        case (Symbol(sym)::Nil, e) =>
          environment.getValueOption(sym).getOrElse(Failure("Lookup Error", s"Value $sym not found in $prefix env.")) -> e
        case (Quote(Symbol(sym))::alt::Nil, e) =>
          environment.getValueOption(sym).getOrElse(alt) -> e
        case (other, e) => Failure("Argument Error", s"Invalid argument: $other") -> e
      }

    )), environment)
  }


  lazy val preludeEnvironment = CombineEnv(Seq(primitiveEnvironment))
}
