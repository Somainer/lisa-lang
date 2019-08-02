import scala.util.Try

object Evaluator {
  import LispExp._
  import SimpleLispTree._
  import Environments._
  trait EvalResult {
    def flatMap(fn: Expression => EvalResult): EvalResult
    def isSuccess: Boolean
  }
  case class EvalSuccess(expression: Expression, env: Environment) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = fn(expression)

    override def isSuccess: Boolean = true
  }
  case class EvalFailure(message: String) extends EvalResult {
    override def flatMap(fn: Expression => EvalResult): EvalResult = this

    override def isSuccess: Boolean = false
  }

  def compile(tree: SimpleLispTree): Expression = tree match {
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
      case Value("define")::xs => xs match {
        case Value(sym)::expr::Nil => Define(Symbol(sym), compile(expr))
        case _ => Failure("Syntax Error", "Cannot define a variable like that.")
      }
      case Value("lambda" | "λ")::xs => xs match {
        case SList(params)::sExpr::Nil => params match {
          case list: List[Value] => Closure(compile(sExpr), list.map(compile(_).asInstanceOf[Symbol]))
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
    def pure(evalResult: EvalResult) = evalResult match {
      case EvalSuccess(expression, _) => pureValue(expression)
      case f => f
    }
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
      case Define(Symbol(sym), expr) => eval(expr, env) match {
        case EvalSuccess(result, _) => unit(env.withValue(sym, result))
        case f => f
      }
      case c: Closure => pureValue(c)
      case fn: PrimitiveFunction => pureValue(fn)
      case s: SString => pureValue(s)
      case i: SInteger => pureValue(i)
      case q: Quote => pureValue(q)

      case Apply(func, args) => apply(func, args, env)
      case SIfElse(predicate, consequence, alternative) =>
        eval(predicate, env) match {
          case EvalSuccess(SBool(b), _) => eval(if(b) consequence else alternative, env)
          case f: EvalFailure => f
          case other => EvalFailure(s"Unexpected if predicate value: $other")
        }

      case f => EvalFailure(s"Unexpected: $f")
    }
  }

  def apply(procedure: Expression, arguments: List[Expression], env: Environment): EvalResult = {
//    println(s"Apply $procedure($arguments) with env $env")
//    println(s"EVAL: $procedure => ${eval(procedure, env)}")
    eval(procedure, env) flatMap {
      case Closure(body, boundVars) => {
        if (boundVars.length != arguments.length)
          EvalFailure(s"Function expected ${boundVars.length} args but ${arguments.length} found.")
        else {
          val evaluatedArgs = arguments.map(eval(_, env))
          if(evaluatedArgs.forall(_.isSuccess))
            eval(body, env.newFrame
              .withValues(boundVars.map(_.value).zip(evaluatedArgs.map(_.asInstanceOf[EvalSuccess].expression))))
          else {
            evaluatedArgs.find(!_.isSuccess).get
          }
        }
      }
      case PrimitiveFunction(fn) => Try {
        val evaluatedArgs = arguments.map(eval(_, env))
        if(evaluatedArgs.forall(_.isSuccess))
          fn(evaluatedArgs.map(_.asInstanceOf[EvalSuccess].expression))
        else {
          throw new RuntimeException(evaluatedArgs.find(!_.isSuccess).get.asInstanceOf[EvalFailure].message)
        }
      }.fold(th => EvalFailure(th.getLocalizedMessage), eval(_, env))
    }
  }
}
