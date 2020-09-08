package moe.roselia.lisa

import java.util.NoSuchElementException

import scala.annotation.tailrec
import scala.util.Try
import Util.ReflectionHelpers.{collectException, tryApplyOnObjectReflective, tryToEither}

object Evaluator {
  import Environments._
  import LispExp._
  import SimpleLispTree._
  import Util.ConsoleColor.Implicits._

  val shouldOptimizeTailCall: Boolean = true

  trait InterpreterControlFlow {
    def appendTrace(msg: => String): InterpreterControlFlow
  }
  trait EvalResult extends InterpreterControlFlow {
    @`inline` def flatMap[T <: InterpreterControlFlow](fn: Expression => T): T
    @`inline` def flatMapWithEnv[T <: InterpreterControlFlow](fn: (Expression, Environment) => T): T
    def isSuccess: Boolean
  }
  case class EvalSuccess(expression: Expression, env: Environment) extends EvalResult {
    override def flatMap[T <: InterpreterControlFlow](fn: Expression => T): T = fn(expression)

    override def flatMapWithEnv[T <: InterpreterControlFlow](fn: (Expression, Environment) => T): T =
      fn(expression, env)

    override def isSuccess: Boolean = true

    override def appendTrace(msg: => String): EvalSuccess = this

  }
  case class EvalFailure(message: String, jvmTrace: List[String] = Nil, lisaTrace: List[String] = Nil) extends EvalResult {
    override def flatMap[T <: InterpreterControlFlow](fn: Expression => T): T = this.asInstanceOf[T]

    override def flatMapWithEnv[T <: InterpreterControlFlow](fn: (Expression, Environment) => T): T =
      this.asInstanceOf[T]

    override def appendTrace(msg: => String): EvalFailure =
      copy(lisaTrace = msg :: lisaTrace)

    override def isSuccess: Boolean = false

    override def toString: String = {
      val sb = new StringBuilder(s"Failure: $message\n")
      if (jvmTrace.nonEmpty) {
        val jvmTraceMessage = jvmTrace.map(x => s"\t$x").mkString("\n")
        sb.append("JVM Trace:\n").append(jvmTraceMessage).append("\n")
      }
      if (lisaTrace.nonEmpty) {
        val lisaTraceMessage = lisaTrace.reverse.map(x => s"\t$x").mkString("\n")
        sb.append("Lisa Trace:\n").append(lisaTraceMessage)
      }
      sb.result()
    }
  }

  object EvalFailure {
    def fromThrowable(throwable: Throwable): EvalFailure = {
      // throwable.printStackTrace()
      EvalFailure(throwable.toString, throwable.getStackTrace.map(_.toString).toList)
    }
  }

  object EvalFailureMessage {
    def unapply(arg: EvalFailure): Option[String] = Some(arg.message)
  }

  case class ReplaceStack(expression: Expression, environment: Environment) extends InterpreterControlFlow {
    override def appendTrace(msg: => String) = this
  }

  private val compilePrimitives: PartialFunction[SimpleLispTree, Expression] = {
    case PrecompiledSExpression(p) => p
    case SQuote(exp) => Quote(compileToList(exp))
    case GraveAccentAtom(value) => GraveAccentSymbol(value)
    case Value("true") => SBool(true)
    case Value("false") => SBool(false)
    case Value("null") => JVMNull
    case SUnQuote(q) => UnQuote(compileToList(q))
    case SAtomLeaf(atom) => LispExp.SAtom(atom)
    case Value(value) => {
      if(value.matches("-?\\d+"))
        SInteger(LisaInteger(value))
      //        value.toIntOption.map(SInteger).getOrElse(SFloat(value.toDouble))
      else if(value.matches("""-?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?"""))
        SFloat(LisaDecimal(value))
      else Symbol(value)
    }
    case StringLiteral(value) =>
      SString(value)
    case StringTemplate("$", part :: Nil, Nil) => SString(part)
  }

  private def compileStringTemplate(template: StringTemplate, toList: Boolean): Expression = {
    val StringTemplate(templateName, parts, arguments) = template
    val args = if (toList) arguments.map(compileToList) else arguments.map(compile)
    val stringParts = parts.map(SString)
    val compiled = if (templateName == "$") {
      Apply(Symbol("string"),
        stringParts.head :: args.zip(stringParts.tail).flatten(x => List(x._1, x._2)))
    } else {
      Apply(Symbol(templateName), LisaList(stringParts) :: Apply(Symbol("list"), args) :: Nil)
    }
    if (toList) compiled.toRawList else compiled
  }

  def compileToList(tree: SimpleLispTree): Expression =
    compilePrimitives.applyOrElse[SimpleLispTree, Expression](tree, {
      case SList(ls) => LisaList(ls.map(compileToList).toList)
      case template: StringTemplate => compileStringTemplate(template, toList = true)
    })

  def unQuoteList(ll: Expression): Expression = {
    def backToSimpleLispTree(ex: Expression): SimpleLispTree = ex match {
      case Symbol(sym) => Value(sym)
      case NilObj => SList(Nil)
      case lll: LisaList[_] => SList(lll.map(backToSimpleLispTree))
      case e => PrecompiledSExpression(e)
    }
    compile(backToSimpleLispTree(ll))
  }

  def compile(tree: SimpleLispTree): Expression = tree match {
    case t if compilePrimitives.isDefinedAt(t) => compilePrimitives(t)
    case template: StringTemplate => compileStringTemplate(template, toList = false)
    case SList(ls) => ls match {
      case Value("quote" | "'")::xs => xs match {
        case expr::Nil => Quote(compileToList(expr))
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
                list.map(compileToList), sExpr.init.map(compile)))
            case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
          }
        case _ => Failure("Syntax Error", "Cannot define a variable like that.")
      }
      case Value("lambda" | "λ")::xs => xs match {
        case SList(params)::sExpr => params match {
          case list: List[Value] =>
            LambdaExpression(compile(sExpr.last),
              list.map(compileToList), sExpr.init.map(compile))
          case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
        }
        case _ => Failure("Syntax Error", "Error creating a closure.")
      }
      case Value("define-macro")::xs => xs match {
        case SList(Value(name)::patterns)::sExpr =>
          val compiledChain = sExpr.map(compile)
          val simpleMacro = SimpleMacro(patterns.map(compileToList), compiledChain.last, compiledChain.init)
          Define(Symbol(name), simpleMacro.defines match {
            case SString(document)::_ => simpleMacro.withDocString(document)
            case _ => simpleMacro
          })
        case _ => Failure("Syntax Error", "Error defining a macro")
      }
      case Value("let")::xs => xs match {
        case SList(bounds)::sExpr =>
          @annotation.tailrec
          def compileBounds(sList: Seq[SimpleLispTree],
                            acc: Option[List[(Expression, Expression)]] = Some(Nil)): Option[List[(Expression, Expression)]] =
            sList match {
              case Nil => acc.map(_.reverse)
              case SList(pattern::x::Nil)::xs => compileBounds(xs, acc.map((compileToList(pattern), compile(x))::_))
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
        case pred :: cons :: Nil =>
          SIfElse(compile(pred), compile(cons), NilObj)
        case _ => Failure("Syntax Error", "If else expect two or three expressions.")
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
  def evaluate(exp: Expression, env: Environment): InterpreterControlFlow = {
    def pureValue(expression: Expression) = EvalSuccess(expression, env)
    def pure(evalResult: EvalResult) = evalResult flatMap pureValue
    def unit(newEnv: Environment) = EvalSuccess(NilObj, newEnv)
    def sideEffect(evalResult: EvalResult): EvalResult = evalResult match {
      case EvalSuccess(_, newEnvironment) => unit(newEnvironment)
      case f => f
    }
    @`inline` def fillEnv(result: InterpreterControlFlow, env: Environment = env) = result match {
      case EvalSuccess(exp, _) => EvalSuccess(exp, env)
      case e => e
    }

//    println(s"Evaluating: $exp")

    val evalResult = exp match {
      case f: Failure => EvalFailure(s"${f.tp}: ${f.message}")
      case Symbol(sym) => env.getValueOption(sym).map(pureValue).getOrElse {
        val message = new StringBuilder(s"Symbol $sym not found.")
        Util.SimilarSymbolFinder.findSuitableSuggestion(sym, env)
          .map(suggestion => s" Do you mean ${suggestion.underline}?").foreach(message.append)
        EvalFailure(message.result())
      }
      case bool: SBool => pureValue(bool)
      case o@WrappedScalaObject(_) => pureValue(o)
      case NilObj => pureValue(NilObj)
      case sf: SFloat => pureValue(sf)
      case p: PolymorphicExpression => pureValue(p)
      case r: LisaRecord[_] => pureValue(r)
      case m@SimpleMacro(_, _, _) => pureValue(SimpleMacroClosure.fromMacro(m, env))
      case m@SimpleMacroClosure(_, _, _, _) => pureValue(m)
      case Define(Symbol(sym), expr) => eval(expr, env) flatMap { defined =>
        def updateValue(exp: Expression, isOverride: Boolean = false): Environment = {
          val predefined = env getValueOption sym
          val isDeclared = predefined contains PlaceHolder
          if ((isDeclared || isOverride) && env.isMutable(sym)) {
            // This variable is previously defined or we need to manually override it.
            exp match {
              case or: OriginalEnvironment =>
                if(isDeclared) or.defineAtEnvironment()
                else or.defineAtEnvironment(env)
              case _ =>
            }
            env.forceUpdated(sym, exp)
          } else env.withValue(sym, exp)
        }
        def isPreviouslyDefined = env.getValueOption(sym).collect {
          case or: OriginalEnvironment => or.isDefinedAtEnvironment(env)
        }.contains(true)

        defined match {
          case c@Closure(_, _, capturedEnv, _) =>
            //          val notFound = (c.freeVariables - sym).filterNot(capturedEnv.has)
            //          if(notFound.nonEmpty) {
            //            EvalFailure(s"Symbol not found: ${notFound.mkString(", ")}")
            //          } else
            if(env.directHas(sym) || isPreviouslyDefined) unit {
              env.getValueOption(sym).get match {
                case p: PolymorphicExpression => updateValue(p.withExpression(c), isOverride = true)
                case closure: Closure =>
                  updateValue(PolymorphicExpression.create(closure, sym).withExpression(c), isOverride = true)
                case _ => updateValue(PolymorphicExpression.create(c, sym))
              }
            } else { // if(c.freeVariables contains sym)
              val recursiveFrame = capturedEnv.newMutableFrame
              val recursiveClosure = c.copy(capturedEnv=recursiveFrame)
              recursiveFrame.addValue(sym, recursiveClosure)
              unit(updateValue(recursiveClosure))
            } // else unit(updateValue(c))
          case mac: SimpleMacroClosure if env.directHas(sym) || isPreviouslyDefined =>
            unit {
              env.getValueOption(sym).get match {
                case p: PolymorphicExpression => updateValue(p.withExpression(mac), isOverride = true)
                case m: SimpleMacroClosure =>
                  updateValue(PolymorphicExpression.create(m, sym).withExpression(mac), isOverride = true)
                case _ => updateValue(mac)
              }
            }
          case result => unit(updateValue(result))
        }
      }
      case LambdaExpression(body, boundVariable, nestedExpressions) =>
        val closure = Closure(boundVariable, body, env, nestedExpressions)
        pureValue(nestedExpressions match {
          case SString(document)::_ =>
            closure.withDocString(document)
          case _ => closure
        })
      case id: IdenticalLisaExpression => pureValue(id)
      case c: Closure => pureValue(c)
      case fn: PrimitiveFunction => pureValue(fn)
      case s: SString => pureValue(s)
      case i: SInteger => pureValue(i)
//      case q: Quote => pureValue(q)
      case Quote(q) => pureValue(q)
      case ll: LisaListLike[_] => pureValue(ll)
      case UnQuote(q) => eval(q, env)
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
        case m@SimpleMacroClosure(_, _, _, _) =>
          eval(expandMacro(m, args.map(_.toRawList), env), env)
        case PrimitiveMacro(m) =>
          Try {
            val (result, newEnv) = m(args, env)
            eval(result, newEnv)
          }.fold(EvalFailure.fromThrowable, x => x)
        case pe: PolymorphicExpression => {
          def executeArgs(args: List[Expression]) =
            pe.findMatch(args, env).map {
              case (ex, _) => ex match {
                case m: SimpleMacroClosure => eval(expandMacro(m, args.map(_.toRawList), env), env)
                case PrimitiveMacro(fn) => fn(args, env) match {
                  case (exp, env) => eval(exp, env)
                }
                case els => apply(els, args) match {
                  case ev: EvalResult => fillEnv(ev, env)
                  case ReplaceStack(expression, _) =>
                    ReplaceStack(expression, env)
                }
              }
            }.getOrElse(EvalFailure("No matching procedure to apply"))
          if(pe.byName) executeArgs(args)
          else evalList(args, env).map(_.toList).fold(EvalFailure(_), executeArgs)
        }
        case proc => evalList(args, env).fold(EvalFailure(_),
          evaledArguments => apply(proc, evaledArguments.toList) match {
            case er: EvalResult => fillEnv(er, env)
            case ReplaceStack(expression, _) =>
              ReplaceStack(expression, env)
          })
      } match {
        case success@EvalSuccess(_, _) => success
        case _ if func != Symbol(PHRASE_VAR) && env
          .getValueOption(PHRASE_VAR)
          .filter(_.isInstanceOf[MayBeDefined])
          .exists(_.asInstanceOf[MayBeDefined].isDefinedAt(func :: args, env)) =>
          evaluate(Apply(Symbol(PHRASE_VAR), func :: args), env)
        case f => f
      }
      case SIfElse(predicate, consequence, alternative) =>
        evaluate(predicate, env) match {
          case EvalSuccess(SBool(b), _) => evaluate(if(b) consequence else alternative, env)
          case EvalSuccess(other, _) => EvalFailure(s"Unexpected if predicate value: $other: ${other.tpe.name}")
          case f: EvalFailure => f
        }

      case SCond(conditions) =>
        @tailrec
        def evCond(cond: List[(Expression, Expression)]): InterpreterControlFlow = cond match {
          case Nil => EvalFailure(s"No matching case")
          case (pred, conseq)::xs =>
            evaluate(pred, env) match { // Not using flatMap for tailrec
              case EvalSuccess(res, _) => res match {
                case SBool(true) => evaluate(conseq, env)
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

  @scala.annotation.tailrec
  def eval(exp: Expression, env: Environment): EvalResult = {
    evaluate(exp, env) match {
      case result: EvalResult => result
      case ReplaceStack(expression, environment) =>
        eval(expression, environment)
    }
  }

  def applyToEither(procedure: Expression, arguments: List[Expression]): Either[String, Expression] = {
    eval(Apply(procedure, arguments.map(Quote)), EmptyEnv) match {
      case EvalSuccess(exp, _) => Right(exp)
      case EvalFailureMessage(message) => Left(message)
    }
  }

  def evalList(exps: Seq[Expression], env: Environment): Either[String, Seq[Expression]] = {
    val evaledExpr = exps.map(eval(_, env))
    if(evaledExpr.forall(_.isSuccess))
      Right(evaledExpr.map(_.asInstanceOf[EvalSuccess].expression))
    else Left(evaledExpr.find(!_.isSuccess).get.asInstanceOf[EvalFailure].message)
  }

  def apply(procedure: Expression, arguments: List[Expression]): InterpreterControlFlow = {
    def reportError(err: Throwable) = EvalFailure.fromThrowable(err)
    def reportErrorOfString(err: String) = EvalFailure(err)
    def success(exp: Expression) = EvalSuccess(exp, EmptyEnv)
    def tried(exp: => Expression): EvalResult = Try { exp } fold (reportError, success)
//    println(s"Apply $procedure($arguments)")
//    println(s"EVAL: $procedure => ${eval(procedure, env)}")
    procedure match {
      case closure@Closure(boundVariable, body, capturedEnv, sideEffects) => {
//          val boundEnv = capturedEnv.newFrame
//            .withValues(boundVariable.map(_.asInstanceOf[Symbol].value).zip(arguments))
        val boundEnv = matchArgument(boundVariable, arguments, inEnv = capturedEnv).map(Env(_, capturedEnv))
        if (boundEnv.isDefined) {
          sideEffects.foldLeft[EvalResult](EvalSuccess(NilObj, boundEnv.get)) {
            (accumulator, sideEffect) => accumulator flatMapWithEnv {
              (_, env) => eval(sideEffect, env)
            }
          } flatMapWithEnv[InterpreterControlFlow] {
            (_, env) =>
              body match {
                case Apply(fun, arguments) =>
                  eval(fun, env) flatMap { e =>
                    def tailCallOptimized = 
                      evalList(arguments, env)
                        // Quote to prevent duplicate execution on eval
                        .map(_.map(Quote))
                        .map(seq => Apply(e, seq.toList))
                        .map(ReplaceStack(_, env)) match {
                          case Left(_) => evaluate(body, env)
                          case Right(value) => value
                        }
                    e match {
                      case _ if !shouldOptimizeTailCall => evaluate(body, env)
                      case _: Closure => tailCallOptimized
                      case poly: PolymorphicExpression if !poly.byName => tailCallOptimized
                      case _ => evaluate(body, env)
                    }
                 }
                case _ => evaluate(body, env)
              }
          } match {
            case EvalSuccess(result, _) => success(result)
            case f@EvalFailureMessage(_) =>
              f.appendTrace(s"at ${procedure.code}")
            case f => f
          }
        } else reportErrorOfString(s"Match Error, ${genHead(arguments)} does not match ${genHead(boundVariable)}.")
      }

      case PrimitiveFunction(fn) =>
        tried(fn(arguments))
      case WrappedScalaObject(obj) =>
        tryApplyOnObjectReflective(obj, arguments)
          .map(Reflect.ScalaBridge.fromScalaNative)
          .fold(reportError, success)
      case record: LisaRecord[_] =>
        tried(arguments match {
          case Quote(sym @ Symbol(_)) :: Nil => record.apply(sym)
          case (sym@Symbol(_)) :: Nil => record.apply(sym)
        })
      case _ => reportErrorOfString(s"Cannot apply $procedure to $arguments.")
    }
  }

  def expandMacro(m: SimpleMacroClosure, args: Seq[Expression], env: Environment): Expression = {
    def unquote(expression: Expression, env: Environment): Option[Expression] = {
      @`inline` def u(e: Expression) = unquote(e, env)
      def liftOption[T](op: Seq[Option[T]]): Option[Seq[T]] =
        if(op.forall(_.isDefined)) Some(op.map(_.get))
        else None

      def flattenSeq(ex: Seq[Expression]) = ex flatMap {
        case WrappedScalaObject(seq: Seq[Expression]) => seq
        case LisaList(ll) => ll
        case e => Seq(e)
      }

      expression match {
        case LisaList(ll) => liftOption(ll.map {
            case UnQuote(LisaList(ull)) => liftOption(ull.map(u)).map(_.toList).map(LisaList(_))
            case sym@UnQuote(UnQuote(Symbol(_))) => u(sym)
            case ex => u(ex).map(LisaList.fromExpression(_))
          }).map(flattenSeq).map(_.toList).map(LisaList(_))
        case Quote(s) => u(s).map(Quote)
        case UnQuote(Symbol(sym)) => env.getValueOption(sym)
        case UnQuote(other) => u(other)
        case otherwise => Some(otherwise)
      }
    }
    val SimpleMacroClosure(paramsPattern, body, defines, capturedEnv) = m
    val compoundEnvironment = capturedEnv.withValue("dynamic-resolve", PrimitiveMacro {
      case (Symbol(sym) :: Nil, e) => env.getValueOption(sym).getOrElse(throw new NoSuchElementException(sym)) -> e
      case _ => throw new IllegalArgumentException(s"dynamic-resolve only accept one symbol.")
    })
    val evalResult = matchArgument(paramsPattern.toList, args.toList, inEnv = env).map(Env(_, compoundEnvironment)).map(newEnv => {
      defines.foldLeft[EvalResult](EvalSuccess(NilObj, newEnv)) {
        case (accumulator, define) => accumulator flatMapWithEnv {
          case (_, e) => eval(define, e)
        }
      }.flatMapWithEnv {
        case (_, e) => eval(body, e)
      }
    }).map(_.flatMapWithEnv {
      case (exp, env) => exp match {
        case e => unquote(e, env).map(unQuoteList).map(EvalSuccess(_, env)).getOrElse(EvalFailure("Can not expand macro."))
//        case _ => EvalSuccess(exp, env)
      }
    })

    val result = evalResult match {
      case Some(f@EvalFailureMessage(_)) => Failure("Macro Expansion Error", f.appendTrace(s"in expanding: ${m.code}").message)
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
      case LisaList(Symbol("seq") :: args)::xs
        if arguments.headOption.exists(l => l.isInstanceOf[WrappedScalaObject[Seq[Any]]] || l.isInstanceOf[LisaListLike[_]])
      =>
        arguments match {
          case WrappedScalaObject(s: Seq[Any])::ys =>
            for {
              listMatch <- matchArgument(args, s.map(Reflect.ScalaBridge.fromScalaNative).toList, matchResult, inEnv)
              restMatch <- matchArgument(xs, ys, matchResult, inEnv)
            } yield listMatch ++ restMatch
          case (ll: LisaListLike[Expression]) :: ys =>
            for {
              listMatch <- matchArgument(args, ll.list, matchResult, inEnv)
              restMatch <- matchArgument(xs, ys, matchResult, inEnv)
            } yield listMatch ++ restMatch
          case _ => None
        }
      case LisaList(Symbol(ctrl@("?" | "when" | "when?")) :: arg::Nil)::Nil => arguments match {
        case Nil => eval(unQuoteList(arg), MutableEnv(matchResult, inEnv)) match {
          case EvalSuccess(SBool(b), _) => if (b) Some(matchResult.toMap) else None
          case EvalSuccess(exp, _) =>
            throw new IllegalArgumentException(s"Matching guard must returns a boolean, but $exp: ${exp.tpe.name} found.")
          case EvalFailureMessage(msg) if ctrl != "when?" => throw new ArithmeticException(s"Error in pattern matching: $msg")
          case _ => None // when? means treat exceptions as none.
        }
        case _ => None
      }
      case LisaList(Symbol("...") :: Symbol(sym)::Nil)::xs => sym match {
        case "_" => matchArgument(xs, Nil, matchResult, inEnv)
        case symbol if matchResult.contains(symbol) =>
          matchResult(symbol) match {
            case WrappedScalaObject(`arguments`) | LisaList(`arguments`) => matchArgument(xs, Nil, matchResult, inEnv)
            case _ => None
          }
        case _ =>
//          matchResult.update(sym, WrappedScalaObject(arguments.toIndexedSeq))
          matchResult.update(sym, LisaList(arguments))
          matchArgument(xs, Nil, matchResult, inEnv)
      }
      case LisaList(lls)::xs =>
        @`inline`
        def continueMatch(restArgs: List[Expression], ys: List[Expression]) =  for {
          argsMatch <- matchArgument(lls, restArgs, matchResult, inEnv)
          rest <- matchArgument(xs, ys, matchResult, inEnv)
        } yield argsMatch ++ rest

        arguments match {
          case Apply(yHead, yArgs)::ys => continueMatch(yHead :: yArgs, ys)
          case LisaList(llArg) :: ys => continueMatch(llArg, ys)
          case WrappedScalaObject(seq: Seq[Expression]) :: ys => continueMatch(seq.toList, ys)
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
