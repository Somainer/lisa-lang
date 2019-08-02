object Test {
  import SimpleLispTree._
  def main(args: Array[String]): Unit = {
    @annotation.tailrec def prompt(env: Environments.Environment): Unit = {
      import SExpressionParser._
      val s = scala.io.StdIn.readLine(">")
      var newEnv: Environments.Environment = env
//      println(s"Input: $s")
      val exp = parse(root(sExpression), s)
      if(exp.successful) {
        val expression = exp.get
//        println(expression.repr)
//        println(expression)
        val compiled = Evaluator.compile(expression)
//        println(compiled)
        val evr = Evaluator.eval(compiled, env)
        evr match {
          case Evaluator.EvalSuccess(exp, nenv) => {
            println(exp)
            newEnv = nenv
          }
          case f => println(f)
        }
      } else {
        println("Wrong Expression")
        println(exp)
      }
      exp match {
        case Success(SList(List("quit")), _) =>
          println("Good bye")
        case _ => prompt(newEnv)
      }
    }
    prompt(Preludes.preludeEnvironment)
  }
}
