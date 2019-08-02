object Evaluator {
  import LispExp._
  import SimpleLispTree._
  import Environments._
  trait EvalResult
  case class EvalSuccess(expression: Expression, env: Environment) extends EvalResult
  case class EvalFailure(message: String) extends EvalResult

  def compile(tree: SimpleLispTree): Expression = tree match {
    case Value("true") => SBool(true)
    case Value("false") => SBool(false)
    case Value(value) => Symbol(value)
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
      case Value("lambda")::xs => xs match {
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

    println(s"Evaluating: $exp with Env: $env")

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

      case Apply(func, args) => func match {
        case Closure(body, boundVars) => {
          if (boundVars.length != args.length)
            EvalFailure(s"Function expected ${boundVars.length} args but ${args.length} found.")
          else {
            env.newFrame.withValues(boundVars.map(_.value).zip(args))
            ???
          }
        }
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

  def apply(procedure: Expression, arguments: List[Expression], env: Environment): Expression = {
    ???
  }
}
