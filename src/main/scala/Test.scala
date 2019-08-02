import scala.util.Try
import scala.util.control.NonFatal

object Test {
  import SimpleLispTree._
  def main(args: Array[String]): Unit = {
    @annotation.tailrec def prompt(env: Environments.Environment): Unit = {
      import SExpressionParser._
      val s = scala.io.StdIn.readLine("Prelude>")
      if (s.nonEmpty) {
        //      println(s"Input: $s")
        val exp = parse(root(sExpression), s)
        exp match {
          case Success(expression, _) => expression match {
            case SList(List(Value("quit" | "exit"))) =>
              println("Good bye")
            case _ =>
              Try(Evaluator.eval(Evaluator.compile(expression), env))
                .recover{
                  case NonFatal(ex) => Evaluator.EvalFailure(ex.getLocalizedMessage)
                }.get match {
                case Evaluator.EvalSuccess(result, newEnv) =>
                  result match {
                    case LispExp.Failure(typ, msg) =>
                      println(s"$typ: $msg")
                    case s => println(s)
                  }
                  prompt(newEnv)
                case f =>
                  println(s"Runtime Error: $f")
                  prompt(env)
              }
          }
          case f =>
            println("Parse Error")
            println(f)
            prompt(env)
        }
      } else prompt(env)

    }
    prompt(Preludes.preludeEnvironment)
  }
}
