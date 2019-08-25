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

  trait MayBeDefined {
    protected def pattern: List[List[Expression]] = Nil

    def isDefinedAt(input: List[Expression], env: Environment): Boolean = {
      pattern.exists(Evaluator.matchArgument(_, input, inEnv = env).isDefined)
    }
  }

  trait WithFreeValues {
    def freeVariables: Set[String] = collectEnvDependency(Set.empty)._1
    def freeVariables(env: Environment) = collectEnvDependency(Set.empty, env, Nil)._1
    def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      // (Dependency, NewDefined)
      collectEnvDependency(defined, EmptyEnv, Nil)
    }
    def collectEnvDependency(defined: Set[String], env: Environment, context: List[Expression] = Nil): (Set[String], Set[String]) =
      collectEnvDependency(defined)
    protected def accumulateDependencies(expressions: List[Expression], initial: Set[String]) =
      expressions.foldLeft(Set.empty[String] -> initial) {
        case ((dependency, defined), expression) =>
          val (deps, defs) = expression.collectEnvDependency(defined)
          (dependency ++ deps, defined ++ defs)
      }
  }

  trait NoExternalDependency extends WithFreeValues {
    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      (Set.empty, defined)
    }

  }

  sealed trait Expression extends DocumentAble with WithFreeValues {
    def valid = true

    def code: String = toString
  }

  case class Symbol(value: String) extends Expression {
    override def toString: String = value

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      val dep = if (defined contains value) Set.empty[String] else Set(value)
      dep -> defined
    }
  }

  class SNumber[T](number: T)(implicit evidence: scala.math.Numeric[T]) extends Expression with NoExternalDependency {
    override def toString: String = number.toString
    def ops = evidence
  }

  case class SInteger(value: Int) extends SNumber(value) with NoExternalDependency

  case class SFloat(value: Double) extends SNumber(value) with NoExternalDependency

  case class SBool(value: Boolean) extends Expression with NoExternalDependency {
    override def toString: String = value.toString
  }

  case class SString(value: String) extends Expression with NoExternalDependency {
    override def toString: String = value.toString

    override def code: String = {
      import scala.reflect.runtime.universe._
      Literal(Constant(value)).toString()
    }
  }

  case object NilObj extends Expression with NoExternalDependency {
    override def toString: String = "( )"
  }

  case class WrappedScalaObject[+T](obj: T) extends Expression with NoExternalDependency {
    def get: T = obj

    override def toString: String = s"#Scala($obj)"
  }

  trait Procedure extends Expression

  case class PrimitiveFunction(function: List[Expression] => Expression)
    extends Procedure with DeclareArityAfter with NoExternalDependency {
    override def toString: String = s"#[Native Code]($function)"
  }
  case class SideEffectFunction(function: (List[Expression], Environment) => (Expression, Environment))
    extends Procedure with NoExternalDependency {
    override def toString: String = "#[Native Code!]"
  }

  case class LambdaExpression(body: Expression, boundVariable: List[Expression],
                              nestedExpressions: List[Expression] = List.empty) extends Expression {
    override def valid: Boolean = body.valid

    override def code: String =
      s"(lambda ${genHead(boundVariable)} ${nestedExpressions.appended(body).map(_.code).mkString(" ")})"

    override def collectEnvDependency(defined: Set[String],
                                      environment: Environment,
                                      context: List[Expression]): (Set[String], Set[String]) = {
      val innerDefinition = getBoundVariables(boundVariable) ++ defined
      val guardDependency = boundVariable.lastOption.map {
        case Apply(Symbol("?" | "when" | "when?"), xs::Nil) =>
          xs.collectEnvDependency(innerDefinition)._1
        case _ => Set.empty[String]
      }.getOrElse(Set.empty)
      val (dependency, defines) = nestedExpressions.foldLeft(Set.empty[String] -> (innerDefinition)) {
        case ((dependency, defined), expression) =>
          val (deps, defs) = expression.collectEnvDependency(defined, environment, context)
          val newDef = defs -- defined
          (dependency ++ deps -- newDef, defined ++ defs)
      }
      (dependency ++ guardDependency ++ body.collectEnvDependency(defines)._1) -> defined
    }
  }

  case class Closure(boundVariable: List[Expression],
                     body: Expression,
                     capturedEnv: Environments.Environment,
                     sideEffects: List[Expression] = List.empty) extends Procedure with MayHaveArity with MayBeDefined {
    override def valid: Boolean = body.valid

    override def toString: String = s"#Closure[${genHead(boundVariable)}]"

    override def docString: String = s"${genHead(boundVariable)}: ${if(document.isEmpty) code else document}"

    def copy(boundVariable: List[Expression] = boundVariable,
             body: Expression = body,
             capturedEnv: Environments.Environment = capturedEnv,
             sideEffects: List[Expression] = sideEffects): Closure =
      Closure(boundVariable, body, capturedEnv, sideEffects).withDocString(document)

    private lazy val rawLambdaExpression = LambdaExpression(body, boundVariable, sideEffects)

    override def code: String = rawLambdaExpression.code

    override lazy val arity: Option[Int] = getArityOfPattern(boundVariable)

    override def pattern: List[List[Expression]] = boundVariable::Nil

    override def isDefinedAt(input: List[Expression], _env: Environment): Boolean =
      super.isDefinedAt(input, capturedEnv)

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      rawLambdaExpression collectEnvDependency defined
  }

  case class SIfElse(predicate: Expression, consequence: Expression, alternative: Expression) extends Procedure {
    override def valid: Boolean = predicate.valid && consequence.valid && alternative.valid

    override def code: String = s"(if ${predicate.code} ${consequence.code} ${alternative.code})"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      accumulateDependencies(predicate::consequence::alternative::Nil, defined)
  }
  case class SCond(conditions: List[(Expression, Expression)]) extends Expression {
    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      accumulateDependencies(conditions.flatMap(it => it._1::it._2::Nil), defined)
  }

  case class Apply(head: Expression, args: List[Expression]) extends Expression {
    override def valid: Boolean = head.valid && args.forall(_.valid)

    override def code: String =
      if(args.isEmpty) s"(${head.code})" else s"(${head.code} ${args.map(_.code).mkString(" ")})"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      accumulateDependencies(head::args, defined)
    }

    override def collectEnvDependency(defined: Set[String],
                                      env: Environment,
                                      context: List[Expression]): (Set[String], Set[String]) = {
      val (lastDeps, lastDefs) = collectEnvDependency(defined)
      head match {
        case Symbol(sym) if env.has(sym) =>
          val (deps, defs) = env.getValueOption(sym).get.collectEnvDependency(defined, env, args)
          (lastDeps ++ deps, lastDefs ++ defs)
        case _ => lastDeps -> lastDefs
      }
    }
  }

  case class Define(symbol: Expression, value: Expression) extends Expression {
    override def valid: Boolean = value.valid

    override def code: String = s"(define ${symbol.code} ${value.code})"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      val (deps, defs) = value.collectEnvDependency(defined)
      val newDef = symbol match {
        case Symbol(sym) => defs + sym
        case _ => defs
      }
      deps -> newDef
    }
  }

  case class Quote(exp: Expression) extends Expression with NoExternalDependency {
    override def valid: Boolean = exp.valid

    override def toString: String = s"'${exp.toString}"

    override def code: String = s"'${exp.code}"
  }

  case class UnQuote(quote: Expression) extends Expression with NoExternalDependency {
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

  def getBoundVariables(pat: List[Expression]): Set[String] = pat match {
    case Nil => Set.empty
    case Apply(Symbol("..."), Symbol(sym)::Nil)::Nil => Set(sym)
    case Apply(Symbol("?" | "when?" | "when"), _)::Nil => Set.empty
    case Symbol("_")::xs => getBoundVariables(xs)
    case Symbol(sym)::xs => getBoundVariables(xs) + sym
    case Apply(Symbol("seq"), args)::xs => getBoundVariables(xs) ++ getBoundVariables(args)
    case Apply(ex, args)::xs => getBoundVariables(ex::xs) ++ getBoundVariables(args)
    case _::xs => getBoundVariables(xs)
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
                         defines: Seq[Expression])
    extends Expression with MayHaveArity with MayBeDefined with NoExternalDependency {
    override def valid: Boolean = paramsPattern.forall(_.valid) && body.valid && defines.forall(_.valid)

    override def toString: String = s"#Macro(${paramsPattern.mkString(" ")})"

    override def code: String = LambdaExpression(body, paramsPattern.toList, defines.toList).code

    override lazy val arity: Option[Int] = getArityOfPattern(paramsPattern.toList)

    override def pattern: List[List[Expression]] = paramsPattern.toList::Nil
  }

  case class PrimitiveMacro(fn: (List[Expression], Environment) => (Expression, Environment))
    extends Expression with DeclareArityAfter with NoExternalDependency {
    override def toString: String = s"#Macro![Native Code]"
  }

  case class Failure(tp: String, message: String) extends Expression with NoExternalDependency {
    override def valid: Boolean = false
  }

  case class PolymorphicExpression(name: String,
                                   variants: Seq[(Expression, Seq[Expression])],
                                   innerEnvironment: MutableEnv, byName: Boolean=false)
    extends Expression with MayHaveArity with MayBeDefined {
    def findMatch(args: Seq[Expression],
                  inEnv: Environment = EmptyEnv): Option[(Expression, Map[String, Expression])] = {
      @annotation.tailrec
      def find(v: List[(Expression, Seq[Expression])]): Option[(Expression, Map[String, Expression])] = v match {
        case Nil => None
        case (exp, mat)::xs =>
          val env = exp match {
            case Closure(_, _, capturedEnv, _) => capturedEnv
            case _ => inEnv
          }
          Evaluator.matchArgument(mat, args, inEnv = env) match {
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
        val nc =
          if(c.freeVariables.contains(name)) c.copy(capturedEnv=CombineEnv.of(innerEnvironment, capturedEnv))
          else c
        val newPolymorphic = copy(variants=variants.appended((nc, nc.boundVariable)))
        innerEnvironment.addValue(name, newPolymorphic)
        newPolymorphic
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

    override def isDefinedAt(input: List[Expression], env: Environment): Boolean = findMatch(input, env).isDefined

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      accumulateDependencies(variants.map(_._1).toList, defined + name)
    }
  }

  object PolymorphicExpression {
    def create(closure: Closure, name: String): PolymorphicExpression = {
      val sharedEnv = Environments.EmptyEnv.newMutableFrame
      val recursiveClosure = closure.copy(capturedEnv =
        Environments.CombineEnv.of(sharedEnv, closure.capturedEnv))
      val polymorphic =
        PolymorphicExpression(name, Seq((recursiveClosure, recursiveClosure.boundVariable)), sharedEnv)
      sharedEnv.addValue(name, polymorphic)
      polymorphic
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
