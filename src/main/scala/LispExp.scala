object LispExp {
  sealed trait Expression {
    def valid = true
  }
  case class Symbol(value: String) extends Expression
  case class SInteger(value: Int) extends Expression
  case class SFloat(value: Double) extends Expression
  case class SBool(value: Boolean) extends Expression
  case class SString(value: String) extends Expression
  case object NilObj extends Expression

  trait Procedure extends Expression
  case class PrimitiveFunction(function: List[Expression] => Expression) extends Procedure
  case class Closure(body: Expression, boundVariable: List[Symbol]) extends Procedure {
    override def valid: Boolean = body.valid
  }
  case class SIfElse(predicate: Expression, consequence: Expression, alternative: Expression) extends Procedure {
    override def valid: Boolean = predicate.valid && consequence.valid && alternative.valid
  }

  case class Apply(head: Expression, args: List[Expression]) extends Expression {
    override def valid: Boolean = head.valid && args.forall(_.valid)
  }
  case class Define(symbol: Symbol, value: Expression) extends Expression {
    override def valid: Boolean = value.valid
  }
  case class Quote(exp: Expression) extends Expression {
    override def valid: Boolean = exp.valid
  }

  case class Failure(tp: String, message: String) extends Expression {
    override def valid: Boolean = false
  }

}