import moe.roselia.lisa
import lisa.LispExp._

trait ExpressionHelper extends Implicits {
  def define(sym: Expression, body: Expression) = Define(sym, body)

  def lambdaExpression(args: Seq[Expression], body: Expression*) =
    LambdaExpression(body.last, args.toList, body.init.toList)

  def defun(sym: Symbol, args: Seq[Expression], body: Expression*): Define =
    define(sym, lambdaExpression(args, body: _*))

  implicit class StringHelper(sc: StringContext) {
    def symbol(arg: Any*) = Symbol(sc.s(arg: _*))
    def lisa(arg: Any*): Expression = sc.s(arg: _*).toLisa
  }

  implicit class ExpressionOps(ex: Expression) {
    def apply(args: Expression*): Expression = Apply(ex, args.toList)
    def quoted = Quote(ex)
  }

  implicit class StringOps(string: String) {
    def asSymbol = Symbol(string)
    def asAtom = SAtom(string)
    def toLisa: Expression = {
      val program = string
      import moe.roselia.lisa.SExpressionParser._
      import moe.roselia.lisa.LispExp
      val lisp = parseAll(sExpression, program).get
      moe.roselia.lisa.Evaluator.compile(lisp).ensuring(!_.isInstanceOf[LispExp.Failure])
    }
  }

  implicit class AnyToLisa[T](value: T)(implicit lift: T => Expression) {
    def asLisa: Expression = lift(value)
  }

}
