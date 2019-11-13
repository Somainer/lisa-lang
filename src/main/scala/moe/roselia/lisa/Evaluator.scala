package moe.roselia.lisa

import scala.annotation.tailrec
import scala.util.Try

object Evaluator {
  import Environments._
  import LispExp._
  import SimpleLispTree._
  trait EvalResult {
    def flatMap(fn: Expression => EvalResult): EvalResult
    def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult
    def isSuccess: Boolean
    def appendTrace(msg: => String): EvalResult
  }
  case class EvalSuccess(expression: Expression, env: Environment) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = fn(expression)

    override def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult =
      fn(expression, env)

    override def isSuccess: Boolean = true

    override def appendTrace(msg: => String): EvalSuccess = this
  }
  case class EvalFailure(message: String) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = this

    override def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult = this

    override def appendTrace(msg: => String): EvalFailure = copy(s"$message\n\t$msg")

    override def isSuccess: Boolean = false

    override def toString: String = s"Failure: $message"
  }

  object EvalFailure {
    def fromThrowable(throwable: Throwable): EvalFailure = {
      throwable.printStackTrace()
      EvalFailure(throwable.toString)
    }

    def apply(message: String): EvalFailure = new EvalFailure(message)

    def apply(throwable: Throwable): EvalFailure = fromThrowable(throwable)
  }

  def compile(tree: SimpleLispTree): Expression = tree match {
    case PrecompiledSExpression(p) => p
    case SQuote(exp) => Quote(compile(exp))
    case Value("true") => SBool(true)
    case Value("false") => SBool(false)
    case SUnQuote(q) => UnQuote(compile(q))
    case GraveAccentAtom(value) => GraveAccentSymbol(value)
    case Value(value) => {
      if(value.matches("-?\\d+"))
        value.toIntOption.map(SInteger).getOrElse(SFloat(value.toDouble))
      else if(value.matches("""([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?""")) SFloat(value.toDouble)
      else Symbol(value)
    }
    case StringLiteral(value) => SString(value)
    case SList(ls) => ls match {
      case Value("quote" | "'")::xs => xs match {
        case expr::Nil => Quote(compile(expr))
        case _ => Failure("Syntax Error", "quote expect only one argument.")
      }
      case Value("unquote")::xs => xs match {
        case expr::Nil => UnQuote(compile(expr))
        case _ => Failure("Syntax Error", "unquote expect only one argument.")
      }
      case Value("define")::xs => xs match {
        case Value(sym)::expr::Nil => Define(Symbol(sym), compile(expr))
        case (sym@SUnQuote(_))::expr::Nil => Define(compile(sym), compile(expr))
        case SList(sym::params)::sExpr =>
          params match {
            case list: List[Value] =>
              Define(compile(sym), LambdaExpression(compile(sExpr.last),
                list.map(compile), sExpr.init.map(compile)))
            case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
          }
        case _ => Failure("Syntax Error", "Cannot define a variable like that.")
      }
      case Value("lambda" | "λ")::xs => xs match {
        case SList(params)::sExpr => params match {
          case list: List[Value] =>
            LambdaExpression(compile(sExpr.last),
              list.map(compile), sExpr.init.map(compile))
          case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
        }
        case _ => Failure("Syntax Error", "Error creating a closure.")
      }
      case Value("define-macro")::xs => xs match {
        case SList(Value(name)::patterns)::sExpr =>
          val compiledChain = sExpr.map(compile)
          val simpleMacro = SimpleMacro(patterns.map(compile), compiledChain.last, compiledChain.init)
          Define(Symbol(name), simpleMacro.defines match {
            case SString(document)::_ => simpleMacro.withDocString(document)
            case _ => simpleMacro
          })
        case _ => Failure("Syntax Error", "Error define a macro")
      }
      case Value("let")::xs => xs match {
        case SList(bounds)::sExpr =>
          @annotation.tailrec
          def compileBounds(sList: Seq[SimpleLispTree],
                            acc: Option[List[(Symbol, Expression)]] = Some(Nil)): Option[List[(Symbol, Expression)]] =
            sList match {
              case Nil => acc.map(_.reverse)
              case SList(Value(sym)::x::Nil)::xs => compileBounds(xs, acc.map((Symbol(sym), compile(x))::_))
              case _ => None
            }
          compileBounds(bounds).map(cBounds => {
            val cExpr = sExpr.map(compile)
            Apply(LambdaExpression(cExpr.last, cBounds.map(_._1), cExpr.init), cBounds.map(_._2))
          }).getOrElse(Failure("Let Error", "Error parsing bounding variables"))
      }
      case Value("if")::xs => xs match {
        case pred :: cons :: alt :: Nil =>
          SIfElse(compile(pred), compile(cons), compile(alt))
        case _ => Failure("Syntax Error", "If else expect three expressions.")
      }
      case Value("cond")::xs =>
        @tailrec
        def compileCond(sList: Seq[SimpleLispTree],
                        acc: Option[List[(Expression, Expression)]] = Some(Nil)): Option[List[(Expression, Expression)]] =
          sList match {
            case Nil =>  acc.map(_.reverse)
            case SList(Value("else")::res::Nil)::x => compileCond(x, acc.map((SBool(true), compile(res))::_))
            case SList(pred::cons::Nil)::x  => compileCond(x, acc.map((compile(pred), compile(cons))::_))
            case _ => None
          }
        compileCond(xs).map(SCond).getOrElse(Failure("Syntax Error", "Error parsing cond syntax."))
      case head::tail => Apply(compile(head), tail.map(compile))
      case Nil => NilObj
    }
    case _ => Failure("Compile Error", s"Error compiling: $tree")
  }
  def eval(exp: Expression, env: Environment): EvalResult = {
    def pureValue(expression: Expression) = EvalSuccess(expression, env)
    def pure(evalResult: EvalResult) = evalResult flatMap pureValue
    def unit(newEnv: Environment) = EvalSuccess(NilObj, newEnv)
    def sideEffect(evalResult: EvalResult): EvalResult = evalResult match {
      case EvalSuccess(_, newEnvironment) => unit(newEnvironment)
      case f => f
    }

//    println(s"Evaluating: $exp")

    val evalResult = exp match {
      case f: Failure => EvalFailure(s"${f.tp}: ${f.message}")
      case Symbol(sym) => env.getValueOption(sym).map(pureValue).getOrElse(EvalFailure(s"Symbol $sym not found."))
      case bool: SBool => pureValue(bool)
      case NilObj => pureValue(NilObj)
      case sf: SFloat => pureValue(sf)
      case p: PolymorphicExpression => pureValue(p)
      case o@WrappedScalaObject(_) => pureValue(o)
      case Define(Symbol(sym), expr) => eval(expr, env) flatMap {
        case c@Closure(_, _, capturedEnv, _) =>
          val notFound = (c.freeVariables - sym).filterNot(capturedEnv.has)
          if(notFound.nonEmpty) {
            EvalFailure(s"Symbol not found: ${notFound.mkString(", ")}")
          } else if(env.directHas(sym)) unit {
            env.getValueOption(sym).get match {
              case p: PolymorphicExpression => env.withValue(sym, p.withExpression(c))
              case closure: Closure =>
                env.withValue(sym, PolymorphicExpression.create(closure, sym).withExpression(c))
              case _ => env.withValue(sym, PolymorphicExpression.create(c, sym))
            }
          } else if(c.freeVariables contains sym) {
            val recursiveFrame = capturedEnv.newMutableFrame
            val recursiveClosure = c.copy(capturedEnv=recursiveFrame)
            recursiveFrame.addValue(sym, recursiveClosure)
            unit(env.withValue(sym, recursiveClosure))
          } else unit(env.withValue(sym, c))
        case mac: SimpleMacro if env.directHas(sym) =>
          unit {
            env.getValueOption(sym).get match {
              case p: PolymorphicExpression => env.withValue(sym, p.withExpression(mac))
              case m: SimpleMacro => env.withValue(sym, PolymorphicExpression.create(m, sym).withExpression(mac))
              case _ => env.withValue(sym, mac)
            }
          }
        case result => unit(env.withValue(sym, result))
      }
      case LambdaExpression(body, boundVariable, nestedExpressions) =>
        val closure = Closure(boundVariable, body, env, nestedExpressions)
        pureValue(nestedExpressions match {
          case SString(document)::_ =>
            closure.withDocString(document)
          case _ => closure
        })
      case c: Closure => pureValue(c)
      case fn: PrimitiveFunction => pureValue(fn)
      case s: SString => pureValue(s)
      case i: SInteger => pureValue(i)
      case q: Quote => pureValue(q)
      case UnQuote(q) => eval(q, env) flatMap {
        case Quote(qt) => pureValue(qt)
        case _ => EvalFailure(s"Cannot unquote $q.")
      }
      case m@SimpleMacro(_, _, _) => pureValue(m)
      case pm: PrimitiveMacro => pureValue(pm)

      case Apply(func, args) => eval(func, env) flatMap {
        case SideEffectFunction(fn) =>
          Try {
            evalList(args, env).fold(EvalFailure(_),
              evaledArgs => {
                val applied = fn(evaledArgs.toList, env)
                EvalSuccess(applied._1, applied._2)
              })
          }.fold(EvalFailure.fromThrowable, x => x)
        case m@SimpleMacro(_, _, _) =>
          eval(expandMacro(m, args, env), env)
        case PrimitiveMacro(m) =>
          Try {
            val (result, newEnv) = m(args, env)
            eval(result, newEnv)
          }.fold(EvalFailure(_), x => x)
        case pe: PolymorphicExpression => {
          def executeArgs(args: List[Expression]) =
            pe.findMatch(args, env).map {
              case (ex, _) => ex match {
                case m: SimpleMacro => eval(expandMacro(m, args, env), env)
                case PrimitiveMacro(fn) => fn(args, env) match {
                  case (exp, env) => eval(exp, env)
                }
                case els => apply(els, args).fold(EvalFailure(_), EvalSuccess(_, env))
              }
            }.getOrElse(EvalFailure("No matching procedure to apply"))
          if(pe.byName) executeArgs(args)
          else evalList(args, env).map(_.toList).fold(EvalFailure(_), executeArgs)
        }
        case proc => evalList(args, env).fold(EvalFailure(_),
          evaledArguments => apply(proc, evaledArguments.toList).fold(EvalFailure(_), EvalSuccess(_, env)))
      } match {
        case success@EvalSuccess(_, _) => success
        case _ if func != Symbol(PHRASE_VAR) && env
          .getValueOption(PHRASE_VAR)
          .filter(_.isInstanceOf[MayBeDefined])
          .exists(_.asInstanceOf[MayBeDefined].isDefinedAt(func :: args, env)) =>
          eval(Apply(Symbol(PHRASE_VAR), func :: args), env)
        case f => f
      }
      case SIfElse(predicate, consequence, alternative) =>
        eval(predicate, env) match {
          case EvalSuccess(SBool(b), _) => eval(if(b) consequence else alternative, env)
          case f: EvalFailure => f
          case other => EvalFailure(s"Unexpected if predicate value: $other")
        }

      case SCond(conditions) =>
        @tailrec
        def evCond(cond: List[(Expression, Expression)]): EvalResult = cond match {
          case Nil => EvalFailure(s"No matching case")
          case (pred, conseq)::xs =>
            eval(pred, env) match { // Not using flatMap for tailrec
              case EvalSuccess(res, _) => res match {
                case SBool(true) => eval(conseq, env)
                case SBool(false) => evCond(xs)
                case other => EvalFailure(s"Can not tell $other is true or false.")
              }
              case fail => fail
            }
        }
        evCond(conditions)

      case f => EvalFailure(s"Unexpected: $f")
    }
    evalResult.appendTrace(s"at ${exp.code}")
  }

  def evalList(exps: Seq[Expression], env: Environment): Either[String, Seq[Expression]] = {
    val evaledExpr = exps.map(eval(_, env))
    if(evaledExpr.forall(_.isSuccess))
      Right(evaledExpr.map(_.asInstanceOf[EvalSuccess].expression))
    else Left(evaledExpr.find(!_.isSuccess).get.asInstanceOf[EvalFailure].message)
  }

  def apply(procedure: Expression, arguments: List[Expression]): Either[String, Expression] = {
//    println(s"Apply $procedure($arguments)")
//    println(s"EVAL: $procedure => ${eval(procedure, env)}")
    procedure match {
      case Closure(boundVariable, body, capturedEnv, sideEffects) => {
//          val boundEnv = capturedEnv.newFrame
//            .withValues(boundVariable.map(_.asInstanceOf[Symbol].value).zip(arguments))
        val boundEnv = matchArgument(boundVariable, arguments, inEnv = capturedEnv).map(Env(_, capturedEnv))
        if (boundEnv.isDefined) {
          sideEffects.foldLeft[EvalResult](EvalSuccess(NilObj, boundEnv.get)) {
            (accumulator, sideEffect) => accumulator flatMapWithEnv {
              (_, env) => eval(sideEffect, env)
            }
          } flatMapWithEnv {
            (_, env) => eval(body, env)
          } match {
            case EvalSuccess(result, _) => Right(result)
            case f@EvalFailure(_) =>
              Left(f.appendTrace(s"at ${procedure.code}").message)
          }
        } else Left(s"Match Error, ${genHead(arguments)} does not match ${genHead(boundVariable)}.")
      }

      case PrimitiveFunction(fn) =>
        Try(fn(arguments)).fold(ex => {
          val cause = ex.getCause
          val sb = new StringBuilder
          sb.append(ex.toString)
          if (cause ne null) sb.append(s"(Caused by $cause)")
          Left(sb.toString())
        }, Right(_))
      case WrappedScalaObject(obj) =>
        Try{
          obj.asInstanceOf[Function[Seq[Any], Any]](arguments)
        }.orElse(
          Try {
            obj.asInstanceOf[{def apply(any: Any*): Any}].apply(arguments: _*)
          }
        ).orElse{
          Try {
            arguments match {
              case x::Nil => obj.asInstanceOf[{def apply(any: Any): Any}].apply(x)
              case x::y::Nil => obj.asInstanceOf[{def apply(a1: Any, a2: Any): Any}].apply(x, y)
              case x::y::z::Nil => obj.asInstanceOf[{def apply(a1: Any, a2: Any, a3: Any)}].apply(x, y, z)
            }
          }
        }.map(Reflect.ScalaBridge.fromScalaNative).fold(ex => Left(ex.toString), Right(_))
      case _ => Left(s"Cannot apply $procedure to $arguments.")
    }
  }

  def expandMacro(m: SimpleMacro, args: Seq[Expression], env: Environment): Expression = {
    def unquote(expression: Expression, env: Environment): Option[Expression] = {
      def u(e: Expression) = unquote(e, env)
      def liftOption[T](op: Seq[Option[T]]): Option[Seq[T]] =
        if(op.forall(_.isDefined)) Some(op.map(_.get))
        else None
      def flattenSeq(ex: Seq[Expression]) = ex flatMap {
        case WrappedScalaObject(seq: Seq[Expression]) => seq
        case e => Seq(e)
      }
      expression match {
        case Quote(s) => u(s).map(Quote)
        case UnQuote(Symbol(sym)) => env.getValueOption(sym)
        case UnQuote(other) => u(other)
        case Apply(head, args) =>
          for {
            h <- u(head)
            x <- liftOption(args.map(u))
          } yield Apply(h, flattenSeq(x).toList)
        case LambdaExpression(body, boundVariable, nestedExpressions) =>
          for {
            b <- u(body)
            bv <- liftOption(boundVariable.map(u))
            ne <- liftOption(nestedExpressions.map(u))
          } yield LambdaExpression(b, flattenSeq(bv).toList, flattenSeq(ne).toList)

        case SIfElse(predicate, consequence, alternative) =>
          for {
            p <- u(predicate)
            c <- u(consequence)
            a <- u(alternative)
          } yield SIfElse(p, c, a)
        case SCond(cond) =>
          val o = cond.map {
            case (x, y) => for {ux <- u(x); uy <- u(y)} yield (ux, uy)
          }
          liftOption(o).map(_.toList).map(SCond)
        case Define(sym, value) => for {
          s <- u(sym)
          v <- u(value)
        } yield Define(s.asInstanceOf[Symbol], v)
        case otherwise => Some(otherwise)
      }
    }
    val SimpleMacro(paramsPattern, body, defines) = m
    val evalResult = matchArgument(paramsPattern.toList, args.toList, inEnv = env).map(Env(_, env)).map(newEnv => {
      defines.foldLeft[EvalResult](EvalSuccess(NilObj, newEnv)) {
        case (accumulator, define) => accumulator flatMapWithEnv {
          case (_, e) => eval(define, e)
        }
      }.flatMapWithEnv {
        case (_, e) => eval(body, e)
      }
    }).map(_.flatMapWithEnv {
      case (exp, env) => exp match {
        case Quote(e) => unquote(e, env).map(EvalSuccess(_, env)).getOrElse(EvalFailure("Can not expand macro."))
        case _ => EvalSuccess(exp, env)
      }
    })

    val result = evalResult match {
      case Some(f@EvalFailure(_)) => Failure("Macro Expansion Error", f.appendTrace(s"in expanding: ${m.code}").message)
      case Some(EvalSuccess(exp, _)) => exp
      case None =>
        Failure("Macro Expansion Error", s"Error expanding ${m.code}.")
    }
//      println(s"$m expanded to $result")
    result
  }

  def matchArgument(pattern: List[Expression],
                    arguments: List[Expression],
                    matchResult: collection.mutable.Map[String, Expression] = collection.mutable.Map.empty,
                    inEnv: Environment = EmptyEnv): Option[Map[String, Expression]] =
    pattern match {
      case Nil => if(arguments.isEmpty) Some(matchResult.toMap) else None
      case Symbol("_")::xs => arguments match {
        case _::ys => matchArgument(xs, ys, matchResult, inEnv)
        case Nil => None
      }
      case GraveAccentSymbol(sym)::xs
        if inEnv.has(sym) && !matchResult.contains(sym) =>
        val contextValue = inEnv.getValueOption(sym).get
        arguments match {
          case `contextValue`::ys => matchArgument(xs, ys, matchResult, inEnv)
          case _ => None
        }
      case Symbol(sym)::xs => arguments match {
        case exp::ys =>
          if(matchResult.contains(sym)) {
            if(matchResult(sym) == exp) matchArgument(xs, ys, matchResult, inEnv)
            else None
          } else {
            matchResult.update(sym, exp)
            matchArgument(xs, ys, matchResult, inEnv)
          }
        case _ => None
      }
      case Apply(Symbol("seq"), args)::xs if arguments.headOption.exists(_.isInstanceOf[WrappedScalaObject[Seq[Any]]]) =>
        arguments match {
          case WrappedScalaObject(s: Seq[Any])::ys =>
            for {
              listMatch <- matchArgument(args, s.map(Reflect.ScalaBridge.fromScalaNative).toList, matchResult, inEnv)
              restMatch <- matchArgument(xs, ys, matchResult, inEnv)
            } yield listMatch ++ restMatch
          case _ => None
        }
      case Apply(Symbol(ctrl@("?" | "when" | "when?")), arg::Nil)::Nil => arguments match {
        case Nil => eval(arg, MutableEnv(matchResult, inEnv)) match {
          case EvalSuccess(SBool(b), _) => if (b) Some(matchResult.toMap) else None
          case EvalSuccess(_, _) => throw new IllegalArgumentException("Matching guard must returns a boolean")
          case EvalFailure(msg) if ctrl != "when?" => throw new ArithmeticException(s"Error in pattern matching: $msg")
          case _ => None // when? means treat exceptions as none.
        }
        case _ => None
      }
      case Apply(Symbol("..."), Symbol(sym)::Nil)::xs => sym match {
        case "_" => matchArgument(xs, Nil, matchResult, inEnv)
        case symbol if matchResult.contains(symbol) =>
          matchResult(symbol) match {
            case WrappedScalaObject(`arguments`) => matchArgument(xs, Nil, matchResult, inEnv)
            case _ => None
          }
        case _ =>
          matchResult.update(sym, WrappedScalaObject(arguments.toIndexedSeq))
          matchArgument(xs, Nil, matchResult, inEnv)
      }
      case Apply(head, args)::xs => arguments match {
        case Apply(yHead, yArgs)::ys => for {
          headMatch <- matchArgument(List(head), List(yHead), matchResult, inEnv)
          argsMatch <- matchArgument(args, yArgs, matchResult, inEnv)
          rest <- matchArgument(xs, ys, matchResult, inEnv)
        } yield headMatch ++ argsMatch ++ rest
        case _ => None
      }
      case Quote(Symbol(sym))::xs => arguments match {
        case Symbol(`sym`)::ys => matchArgument(xs, ys, matchResult, inEnv)
        case Quote(Symbol(`sym`))::ys => matchArgument(xs, ys, matchResult, inEnv)
        case _ => None
      }
      case otherwise::xs => arguments match {
        case `otherwise`::ys => matchArgument(xs, ys, matchResult, inEnv)
        case _ => None
      }
    }
}
