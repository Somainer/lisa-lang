package moe.roselia.lisa

import scala.util.Try

object Evaluator {
  import Environments._
  import LispExp._
  import SimpleLispTree._
  trait EvalResult {
    def flatMap(fn: Expression => EvalResult): EvalResult
    def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult
    def isSuccess: Boolean
  }
  case class EvalSuccess(expression: Expression, env: Environment) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = fn(expression)

    override def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult =
      fn(expression, env)

    override def isSuccess: Boolean = true
  }
  case class EvalFailure(message: String) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = this

    override def flatMapWithEnv(fn: (Expression, Environment) => EvalResult): EvalResult = this

    override def isSuccess: Boolean = false
  }

  def compile(tree: SimpleLispTree): Expression = tree match {
    case SQuote(exp) => Quote(compile(exp))
    case Value("true") => SBool(true)
    case Value("false") => SBool(false)
    case Value(value) => {
      if(value.matches("\\d+")) SInteger(value.toInt)
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
        case SList(Value(sym)::params)::sExpr =>
          params match {
            case list: List[Value] =>
              Define(Symbol(sym), LambdaExpression(compile(sExpr.last),
                list.map(compile(_).asInstanceOf[Symbol]), sExpr.init.map(compile)))
            case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
          }
        case _ => Failure("Syntax Error", "Cannot define a variable like that.")
      }
      case Value("lambda" | "λ")::xs => xs match {
        case SList(params)::sExpr => params match {
          case list: List[Value] =>
            LambdaExpression(compile(sExpr.last),
              list.map(compile(_).asInstanceOf[Symbol]), sExpr.init.map(compile))
          case other => Failure("Syntax Error", s"A list of Symbol expected but $other found.")
        }
        case _ => Failure("Syntax Error", "Error creating a closure.")
      }
      case Value("if")::xs => xs match {
        case pred :: cons :: alt :: Nil =>
          SIfElse(compile(pred), compile(cons), compile(alt))
        case _ => Failure("Syntax Error", "If else expect three expressions.")
      }
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

    exp match {
      case f: Failure => pureValue(f)
      case Symbol(sym) => env.getValueOption(sym).map(pureValue).getOrElse(EvalFailure(s"Symbol $sym not found."))
      case bool: SBool => pureValue(bool)
      case NilObj => pureValue(NilObj)
      case Define(Symbol(sym), expr) => eval(expr, env) flatMap {
        case c@Closure(_, _, capturedEnv, _) =>
          val recursiveFrame = capturedEnv.newMutableFrame
          val recursiveClosure = c.copy(capturedEnv=recursiveFrame)
          recursiveFrame.addValue(sym, recursiveClosure)
          unit(env.withValue(sym, recursiveClosure))
        case result => unit(env.withValue(sym, result))
      }
      case LambdaExpression(body, boundVariable, nestedExpressions) =>
        pureValue(Closure(boundVariable, body, env, nestedExpressions))
      case c: Closure => pureValue(c)
      case fn: PrimitiveFunction => pureValue(fn)
      case s: SString => pureValue(s)
      case i: SInteger => pureValue(i)
      case q: Quote => pureValue(q)
      case UnQuote(q) => eval(q, env) flatMap {
        case Quote(qt) => eval(qt, env)
        case _ => EvalFailure(s"Cannot unquote $q.")
      }

      case Apply(func, args) => eval(func, env) flatMap {
        proc => evalList(args, env).fold(EvalFailure,
          evaledArguments => apply(proc, evaledArguments.toList).fold(EvalFailure, EvalSuccess(_, env)))
      }
      case SIfElse(predicate, consequence, alternative) =>
        eval(predicate, env) match {
          case EvalSuccess(SBool(b), _) => eval(if(b) consequence else alternative, env)
          case f: EvalFailure => f
          case other => EvalFailure(s"Unexpected if predicate value: $other")
        }

      case f => EvalFailure(s"Unexpected: $f")
    }
  }

  def evalList(exps: Seq[Expression], env: Environment): Either[String, Seq[Expression]] = {
    val evaledExpr = exps.map(eval(_, env))
    if(evaledExpr.forall(_.isSuccess))
      Right(evaledExpr.map(_.asInstanceOf[EvalSuccess].expression))
    else Left(evaledExpr.find(!_.isSuccess).get.asInstanceOf[EvalFailure].message)
  }

  def apply(procedure: Expression, arguments: List[Expression]): Either[String, Expression] = {
//    println(s"Apply $procedure($arguments) with env $env")
//    println(s"EVAL: $procedure => ${eval(procedure, env)}")
    procedure match {
      case Closure(boundVariable, body, capturedEnv, sideEffects) => {
        if (boundVariable.length != arguments.length)
          Left(s"Function expected ${boundVariable.length} args but ${arguments.length} found.")
        else {
          val boundEnv = capturedEnv.newFrame.withValues(boundVariable.map(_.value).zip(arguments))
          sideEffects.foldRight[EvalResult](EvalSuccess(NilObj, boundEnv)) {
            (sideEffect, accumulator) => accumulator flatMapWithEnv {
              (_, env) => eval(sideEffect, env)
            }
          }.flatMapWithEnv {
            (_, env) => eval(body, env)
          } match {
            case EvalSuccess(result, _) => Right(result)
            case EvalFailure(msg) => Left(msg)
          }
        }
      }
      case PrimitiveFunction(fn) =>
        Try(fn(arguments)).fold(ex => Left(ex.getLocalizedMessage), Right(_))
      case _ => Left(s"Cannot apply $procedure to $arguments.")
    }
  }
}
