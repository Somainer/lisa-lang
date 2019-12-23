package moe.roselia.lisa

import Environments.{CombineEnv, EmptyEnv, Environment, MutableEnv}
import RecordType.{MapRecord, Record}

import scala.annotation.tailrec

object LispExp {

  trait LisaType {
    def name: String
  }

  case class NameOnlyType(name: String) extends LisaType

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

    def tpe: LisaType = NameOnlyType(getClass.getSimpleName)
  }

  trait Symbol extends Expression {
    def value: String

    override def toString: String = value
    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      val dep = if (defined contains value) Set.empty[String] else Set(value)
      dep -> defined
    }
  }

  object Symbol {
    def apply(symbol: String): Symbol = PlainSymbol(symbol)

    def unapply(arg: Symbol): Option[String] = Some(arg.value)
  }

  case class PlainSymbol(value: String) extends Symbol
  case class GraveAccentSymbol(value: String) extends Symbol

  import Numeric.Implicits._
  import Ordering.Implicits._
  import Rational.Implicits._

  type LisaInteger = BigInt
  val LisaInteger = BigInt

  type LisaDecimal = BigDecimal
  val LisaDecimal = BigDecimal

  import LisaInteger.int2bigInt
  import LisaDecimal.double2bigDecimal

  class SNumber[T](val number: T)(implicit evidence: scala.math.Numeric[T])
    extends Expression with Ordered[SNumber[T]] with NoExternalDependency {
    override def toString: String = number.toString
    def mapTo[U : Numeric](implicit transform: T => U): SNumber[U] = SNumber(number)
    def toIntNumber: SNumber[LisaInteger] = SNumber(toRationalNumber.number.toIntegral)
    def toDoubleNumber: SNumber[LisaDecimal] = SNumber(number.toDouble)
    def toRationalNumber: SNumber[Rational[LisaInteger]] = {
      import SNumber.NumberTypes._
      getTypeOrder(number) match {
        case TypeFlags.Integer => SRational(number.asInstanceOf[LisaInteger])
        case TypeFlags.Rational => this.asInstanceOf[SNumber[Rational[LisaInteger]]]
        case _ => Rational.fromDouble[LisaInteger](number.toDouble)
      }
    }
    def +(that: SNumber[T]): SNumber[T] = number + that.number
    def -(that: SNumber[T]): SNumber[T] = number - that.number
    def *(that: SNumber[T]): SNumber[T] = number * that.number
    def /(that: SNumber[T]): SNumber[Any] = {
      import SNumber.NumberTypes._
      val flag = commonLargestType(this, that)
      if (flag <= TypeFlags.Rational) {
        val a = toRationalNumber.number
        val b = that.toRationalNumber.number
        val result = a / b
        if (result.isIntegral) SNumber(result.toIntegral).asInstanceOf[SNumber[Any]]
        else SNumber(result).asInstanceOf[SNumber[Any]]
      }
      else SNumber(number.toDouble / that.number.toDouble).asInstanceOf[SNumber[Any]]
    }
    def unary_- : SNumber[T] = -number
    override def compare(that: SNumber[T]): Int = implicitly[Ordering[T]].compare(number, that.number)
    def max(that: SNumber[T]): SNumber[T] = number max that.number
    def min(that: SNumber[T]): SNumber[T] = number min that.number
    def equalsTo(that: SNumber[T]): Boolean = number equiv that.number

    override def equals(obj: Any): Boolean = obj match {
//      case num: SNumber[T] => equalsTo(num)
      case that: SNumber[_] => SNumber.performComputation(_ equalsTo _)(this, that)
      case other => super.equals(other)
    }

    override def tpe: LisaType = NameOnlyType {
      import SNumber.NumberTypes._
      getTypeOrder(number) match {
        case TypeFlags.Integer => "Integer"
        case TypeFlags.Rational => "Rational"
        case TypeFlags.Double => "Decimal"
      }
    }
  }

  object SNumber {
    implicit def wrapToSNumber[T : Numeric](t: T): SNumber[T] = apply(t)
    def apply[T : Numeric](number: T): SNumber[T] = number match {
      case s: LisaInteger => SInteger(s).asInstanceOf[SNumber[T]]
      case s: LisaDecimal => SFloat(s).asInstanceOf[SNumber[T]]
      case s: Rational[LisaInteger] => SRational(s).asInstanceOf[SNumber[T]]
      case s => new SNumber(s)
    }

    implicit def convertNumberTypes[T, U : Numeric](number: SNumber[T])(implicit transform: T => U): SNumber[U] =
      number.mapTo[U]
    def performComputation[T](f: (SNumber[Any], SNumber[Any]) => T)(a: SNumber[_], b: SNumber[_]): T = {
      val (x, y) = NumberTypes.handleCommonLargestType(a, b)
      f(x, y)
    }
    object NumberTypes {
      object TypeFlags {
        val Integer = 1
        val Rational = 2
        val Double = 3
      }
      def getTypeOrder(obj: Any) = obj match {
        case _: Int | _: Integer | _: LisaInteger => TypeFlags.Integer
        case _: Rational[_] => TypeFlags.Rational
        case _: Float | _: Double | _: java.lang.Double | _: java.lang.Float | _: LisaDecimal =>
          TypeFlags.Double
      }
      def castToType(flag: Int)(obj: SNumber[_]): SNumber[_] = {
        flag match {
          case TypeFlags.Integer => obj.toIntNumber
          case TypeFlags.Rational => obj.toRationalNumber
          case TypeFlags.Double => obj.toDoubleNumber
        }
      }

      def commonLargestType[T, U](a: SNumber[T], b: SNumber[U]) =
        getTypeOrder(a.number) max getTypeOrder(b.number)

      def handleCommonLargestType[T, U](a: SNumber[T], b: SNumber[U]) = {
        val flag = commonLargestType(a, b)
        castToType(flag)(a).asInstanceOf[SNumber[Any]] -> castToType(flag)(b).asInstanceOf[SNumber[Any]]
      }
    }
  }

  case class SInteger(value: LisaInteger) extends SNumber(value)

  case class SFloat(value: LisaDecimal) extends SNumber(value)

  case class SRational(value: Rational[LisaInteger]) extends SNumber(value)

  case class SBool(value: Boolean) extends Expression with NoExternalDependency {
    override def toString: String = value.toString

    override def tpe: LisaType = NameOnlyType("Boolean")
  }

  case class SString(value: String) extends Expression with NoExternalDependency with Ordered[SString] {
    override def toString: String = value.toString

    override def code: String = {
      import scala.reflect.runtime.universe._
      Literal(Constant(value)).toString()
    }

    override def compare(that: SString): Int = value compare that.value
  }

  case object NilObj extends Expression with NoExternalDependency {
    override def toString: String = "( )"
  }

  case class WrappedScalaObject[+T](obj: T) extends Expression with NoExternalDependency {
    require(!obj.isInstanceOf[Failure], "Cannot wrap a failure")
    def get: T = obj

    override def toString: String = s"#Scala($obj)"

    override def tpe: LisaType = NameOnlyType(s"${getClass.getSimpleName}[${obj.getClass.getSimpleName}]")
  }

  object WrappedScalaObject {
    def apply[T](obj: T): WrappedScalaObject[T] = new WrappedScalaObject(obj)
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

    def flattenCaptured: Closure = copy(capturedEnv=capturedEnv.collectValues(freeVariables.toList))
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
          Evaluator.matchArgument(mat.toList, args.toList, inEnv = env) match {
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

    def verboseString = s"#Polymorph$polymorphicType(${variants.length} overloads)(${variants.map(_._2).mkString("|")})"

    override def toString: String = {
      val toStringSymbol = Symbol("to-string")
      val arguments = toStringSymbol :: Nil
      val alternativeArguments = Quote(toStringSymbol) :: Nil
      findMatch(arguments)
        .filter(exp => variants.find(_._1 eq exp._1).exists(v => v._2 == arguments || v._2 == alternativeArguments))
        .flatMap {
          case (exp, _) => Evaluator.apply(exp, arguments).map(_.toString).toOption
        }.getOrElse(verboseString)
//      if (isDefinedAt(Symbol("to-string")::Nil, EmptyEnv))
//        Evaluator.apply(this, Symbol("to-string") :: Nil).toOption.map(_.toString).getOrElse(verboseString)
//      else verboseString
    }

    override lazy val arity: Option[Int] =
      if (variants.length == 1) getArityOfPattern(variants.head._2.toList)
      else {
        val patternArity = variants.map(_._2).map(_.toList).map(getArityOfPattern(_))
        patternArity.distinct.toList match {
          case x :: Nil => x
          case _ => None
        }
      }

    override def isDefinedAt(input: List[Expression], env: Environment): Boolean = findMatch(input, env).isDefined

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      accumulateDependencies(variants.map(_._1).toList, defined + name)
    }

    override def tpe: LisaType =
      if (variants.length == 1) variants.head._1.tpe
      else NameOnlyType(s"Polymorphic$polymorphicType")
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

  trait LisaRecord[V <: Expression] extends Record[String, V] with Expression with NoExternalDependency {
    def apply(sym: Symbol) = selectDynamic(sym.value)
  }

  object LisaRecord {
    def recordMaker(rec: List[Expression], name: String = "") = {
      @scala.annotation.tailrec
      def recurHelper(rec: List[Expression], acc: Map[String, Expression]): Map[String, Expression] = rec match {
        case Nil => acc
        case Symbol(sym)::exp::xs => recurHelper(xs, acc.updated(sym, exp))
        case Quote(Symbol(sym))::exp::xs => recurHelper(xs, acc.updated(sym, exp))
        case x:: _ ::_ => throw new IllegalArgumentException(s"Unrecognized key type: $x: ${x.tpe.name}")
        case x::_ => throw new IllegalArgumentException(s"No matching value for $x")
      }
      LisaMapRecord(recurHelper(rec, Map.empty), name)
    }

    lazy val RecordHelperEnv = Environments.Env(Map(
      "record" -> PrimitiveFunction {
        case SString(name)::xs =>
          recordMaker(xs, name)
        case xs =>
          recordMaker(xs)
      }
    ), EmptyEnv)
  }

  case class LisaMapRecord[V <: Expression](record: Map[String, V], recordTypeName: String = "")
    extends LisaRecord[V] with MapRecord[String, V] {
    override def toString: String = {
      val body = record.map {
        case (k, v) => s"'$k $v"
      }.mkString(" ")
      s"$recordTypeName {$body}".stripLeading
    }

    def updated(key: String, value: V) = copy(record.updated(key, value))
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
