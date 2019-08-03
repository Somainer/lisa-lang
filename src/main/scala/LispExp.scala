object LispExp {
  sealed trait Expression {
    def valid = true
  }
  case class Symbol(value: String) extends Expression {
    override def toString: String = value
  }
  case class SInteger(value: Int) extends Expression {
    override def toString: String = value.toString
  }
  case class SFloat(value: Double) extends Expression {
    override def toString: String = value.toString
  }
  case class SBool(value: Boolean) extends Expression {
    override def toString: String = value.toString
  }
  case class SString(value: String) extends Expression
  case object NilObj extends Expression {
    override def toString: String = "( )"
  }

  trait Procedure extends Expression
  case class PrimitiveFunction(function: List[Expression] => Expression) extends Procedure
  case class LambdaExpression(body: Expression, boundVariable: List[Symbol]) extends Expression {
    override def valid: Boolean = body.valid
  }
  case class Closure(boundVariable: List[Symbol], body: Expression, capturedEnv: Environments.Environment) extends Procedure {
    override def valid: Boolean = body.valid
    override def toString: String = s"#Closure(${boundVariable.mkString(" ")})"
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

    override def toString: String = s"'${exp.toString}"
  }
  case class UnQuote(quote: Expression) extends Expression {
    override def valid: Boolean = quote.valid

    override def toString: String = s"~$quote"
  }

  case class Failure(tp: String, message: String) extends Expression {
    override def valid: Boolean = false
  }

}