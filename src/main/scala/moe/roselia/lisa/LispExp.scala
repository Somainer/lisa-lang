package moe.roselia.lisa

import Environments.{CombineEnv, EmptyEnv, Environment, MutableEnv}

import scala.annotation.tailrec

object LispExp {

  trait DocumentAble {
    var document = ""
    def withDocString(string: String): this.type = {
      document = string
      this
    }

    def copyDocString(that: DocumentAble): this.type = withDocString(that.docString)

    def docString: String = document
  }

  sealed trait Expression extends DocumentAble {
    def valid = true

    def code = toString
  }

  case class Symbol(value: String) extends Expression {
    override def toString: String = value
  }

  class SNumber[T](number: T)(implicit evidence: scala.math.Numeric[T]) extends Expression {
    override def toString: String = number.toString
    def ops = evidence
  }

  case class SInteger(value: Int) extends SNumber(value)

  case class SFloat(value: Double) extends SNumber(value)

  case class SBool(value: Boolean) extends Expression {
    override def toString: String = value.toString
  }

  case class SString(value: String) extends Expression {
    override def toString: String = value.toString

    override def code: String = {
      import scala.reflect.runtime.universe._
      Literal(Constant(value)).toString()
    }
  }

  case object NilObj extends Expression {
    override def toString: String = "( )"
  }

  case class WrappedScalaObject[T](obj: T) extends Expression {
    def get: T = obj

    override def toString: String = s"#Scala($obj)"
  }

  trait Procedure extends Expression

  case class PrimitiveFunction(function: List[Expression] => Expression) extends Procedure with DeclareArityAfter {
    override def toString: String = s"#[Native Code]($function)"
  }
  case class SideEffectFunction(function: (List[Expression], Environment) => (Expression, Environment))
    extends Procedure {
    override def toString: String = "#[Native Code!]"
  }

  case class LambdaExpression(body: Expression, boundVariable: List[Expression],
                              nestedExpressions: List[Expression] = List.empty) extends Expression {
    override def valid: Boolean = body.valid

    override def code: String =
      s"(lambda ${genHead(boundVariable)} ${nestedExpressions.appended(body).map(_.code).mkString(" ")})"
  }

  case class Closure(boundVariable: List[Expression],
                     body: Expression,
                     capturedEnv: Environments.Environment,
                     sideEffects: List[Expression] = List.empty) extends Procedure with MayHaveArity {
    override def valid: Boolean = body.valid

    override def toString: String = s"#Closure[${genHead(boundVariable)}]"

    override def docString: String = s"${genHead(boundVariable)}: ${if(document.isEmpty) code else document}"

    def copy(boundVariable: List[Expression] = boundVariable,
             body: Expression = body,
             capturedEnv: Environments.Environment = capturedEnv,
             sideEffects: List[Expression] = sideEffects): Closure =
      Closure(boundVariable, body, capturedEnv, sideEffects).withDocString(document)

    override def code: String = LambdaExpression(body, boundVariable, sideEffects).code

    override lazy val arity: Option[Int] = getArityOfPattern(boundVariable)
  }

  case class SIfElse(predicate: Expression, consequence: Expression, alternative: Expression) extends Procedure {
    override def valid: Boolean = predicate.valid && consequence.valid && alternative.valid

    override def code: String = s"(if ${predicate.code} ${consequence.code} ${alternative.code})"
  }
  case class SCond(conditions: List[(Expression, Expression)]) extends Expression

  case class Apply(head: Expression, args: List[Expression]) extends Expression {
    override def valid: Boolean = head.valid && args.forall(_.valid)

    override def code: String =
      if(args.isEmpty) s"(${head.code})" else s"(${head.code} ${args.map(_.code).mkString(" ")})"
  }

  case class Define(symbol: Expression, value: Expression) extends Expression {
    override def valid: Boolean = value.valid

    override def code: String = s"(define ${symbol.code} ${value.code})"
  }

  case class Quote(exp: Expression) extends Expression {
    override def valid: Boolean = exp.valid

    override def toString: String = s"'${exp.toString}"

    override def code: String = s"'${exp.code}"
  }

  case class UnQuote(quote: Expression) extends Expression {
    override def valid: Boolean = quote.valid

    override def toString: String = s"~$quote"

    override def code: String = s"~${quote.code}"
  }

  def genHead(ex: Seq[Expression]): String = {
    if (ex.isEmpty) "()"
    else ex.last match {
      case Apply(Symbol("?" | "when"), xs::Nil) => s"${genHead(ex.init)} when ${xs.code}"
      case Apply(Symbol("when?"), xs::Nil) => s"${genHead(ex.init)} when? ${xs.code}"
      case Apply(Symbol("..."), Symbol(x)::Nil) => genHead(ex.init appended Symbol(s"...$x"))
      case _ => s"(${ex.map(_.code).mkString(" ")})"
    }
  }

  @tailrec
  def getArityOfPattern(pat: List[Expression], accumulator: Int = 0): Option[Int] = pat match {
    case Nil => Some(accumulator)
    case Apply(Symbol("..."), _)::Nil => None // Can not count arity on va-args.
    case Apply(Symbol("?" | "when?" | "when"), _)::Nil => Some(accumulator) // Match guards
    case _::xs => getArityOfPattern(xs, accumulator + 1)
  }

  trait MayHaveArity {
    def arity: Option[Int]
  }

  trait DeclareArityAfter extends MayHaveArity {
    private [this] var _arity: Option[Int] = None

    override def arity: Option[Int] = _arity

    def withArity(n: Int): this.type = {
      _arity = Some(n)
      this
    }
  }

  case class SimpleMacro(paramsPattern: Seq[Expression],
                         body: Expression,
                         defines: Seq[Expression]) extends Expression with MayHaveArity {
    override def valid: Boolean = paramsPattern.forall(_.valid) && body.valid && defines.forall(_.valid)

    override def toString: String = s"#Macro(${paramsPattern.mkString(" ")})"

    override def code: String = LambdaExpression(body, paramsPattern.toList, defines.toList).code

    override lazy val arity: Option[Int] = getArityOfPattern(paramsPattern.toList)
  }

  case class PrimitiveMacro(fn: (List[Expression], Environment) => (Expression, Environment)) extends Expression with DeclareArityAfter {
    override def toString: String = s"#Macro![Native Code]"
  }

  case class Failure(tp: String, message: String) extends Expression {
    override def valid: Boolean = false
  }

  case class PolymorphicExpression(name: String,
                                   variants: Seq[(Expression, Seq[Expression])],
                                   innerEnvironment: MutableEnv, byName: Boolean=false) extends Expression with MayHaveArity {
    def findMatch(args: Seq[Expression],
                  inEnv: Environment = EmptyEnv): Option[(Expression, Map[String, Expression])] = {
      @annotation.tailrec
      def find(v: List[(Expression, Seq[Expression])]): Option[(Expression, Map[String, Expression])] = v match {
        case Nil => None
        case (exp, mat)::xs => Evaluator.matchArgument(mat, args, inEnv = inEnv) match {
          case Some(x) => Some((exp, x))
          case _ => find(xs)
        }
      }
      val found = find(variants.toList)
//      if(found.isDefined) println(s"${found.get} matches $args")
      found
    }

    def withExpression(closure: Closure): PolymorphicExpression = closure match {
      case c@Closure(_, _, capturedEnv, _) =>
        val nc = c.copy(capturedEnv=CombineEnv.of(innerEnvironment, capturedEnv))
        val newPolymorph = copy(variants=variants.appended((nc, nc.boundVariable)))
        innerEnvironment.addValue(name, newPolymorph)
        newPolymorph
    }


    def withExpression(mac: SimpleMacro): PolymorphicExpression = {
      val newVariant = copy(variants=variants.appended((mac, mac.paramsPattern)))
      innerEnvironment.addValue(name, newVariant)
      newVariant
    }

    override def docString: String = {
      if(variants.length == 1) {
        variants.head._1.docString
      } else {
        val body = variants.map {
          case (p, v) => if(p.docString.nonEmpty) p.docString else s"${genHead(v)}: ${p.code}"
        }.mkString("\n")
        s"""
           |$name is a polymorphic function, with ${variants.length} overloads:
           |
           |$body
           |""".stripMargin.strip()
      }
    }

    def polymorphicType: String =
      if (variants.length == 1) ""
      else if (byName) "Macro" else "Closure"

    override def toString: String =
      s"#Polymorph$polymorphicType(${variants.length} overloads)(${variants.map(_._2).mkString("|")})"

    override lazy val arity: Option[Int] =
      if (variants.length == 1) getArityOfPattern(variants.head._2.toList)
      else None
  }

  object PolymorphicExpression {
    def create(closure: Closure, name: String): PolymorphicExpression = {
      val sharedEnv = Environments.EmptyEnv.newMutableFrame
      val recursiveClosure = closure.copy(capturedEnv =
        Environments.CombineEnv.of(sharedEnv, closure.capturedEnv))
      val polymorphed =
        PolymorphicExpression(name, Seq((recursiveClosure, recursiveClosure.boundVariable)), sharedEnv)
      sharedEnv.addValue(name, polymorphed)
      polymorphed
    }

    def create(mac: SimpleMacro, name: String): PolymorphicExpression = {
      val sharedEnv = Environments.EmptyEnv.newMutableFrame
      val polymorphicExpression =
        PolymorphicExpression(name, Seq((mac, mac.paramsPattern)), sharedEnv, byName = true)
      sharedEnv.addValue(name, polymorphicExpression)
      polymorphicExpression
    }
  }

  trait Implicits {
    import scala.language.implicitConversions
    implicit def fromInt(i: Int): SInteger = SInteger(i)
    implicit def fromString(s: String): SString = SString(s)
    implicit def fromSymbol(sym: scala.Symbol):Symbol = Symbol(sym.name)
    implicit def fromFloat(f: Float): SFloat = SFloat(f)
    implicit def fromDouble(d: Double): SFloat = SFloat(d)
    implicit def fromBool(b: Boolean): SBool = SBool(b)
    implicit def autoUnit(unit: Unit): NilObj.type = NilObj
  }

  object Implicits extends Implicits

}
