package moe.roselia.lisa

import moe.roselia.lisa.Annotation.RawLisa
import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Environment, MutableEnv}
import moe.roselia.lisa.RecordType.{MapRecord, Record}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.ref.WeakReference

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

  trait WithSourceTree {
    var sourceTree: Option[SimpleLispTree.SimpleLispTree] = None
    def withSourceTree(source: SimpleLispTree.SimpleLispTree): this.type = {
      sourceTree = Some(source)
      this
    }
    def withSourceTree(maybeSource: Option[SimpleLispTree.SimpleLispTree]): this.type = {
      maybeSource.foreach(withSourceTree)
      this
    }
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

  trait CustomHintProvider {
    type HintType = (String, String)
    var hintProvider: String => Seq[HintType] = Function.const(Nil)
    def provideHint(input: String): Seq[HintType] = hintProvider(input)
    def withHintProvider(provider: String => Seq[HintType]): this.type = {
      hintProvider = provider
      this
    }
  }

  trait Expression extends DocumentAble with WithFreeValues with WithSourceTree {
    def valid = true

    def code: String = toString

    def tpe: LisaType = NameOnlyType(getClass.getSimpleName)

    def toRawList: Expression = this
  }

  trait LisaValue

  trait Symbol extends Expression with LisaValue {
    def value: String

    override def toString: String = value

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      val dep = if (defined contains value) Set.empty[String] else Set(value)
      dep -> defined
    }

    override def equals(obj: Any): Boolean = obj match {
      case Symbol(sym) => sym == value
      case x => super.equals(x)
    }
  }

  object Symbol {
    def apply(symbol: String): Symbol = PlainSymbol(symbol)

    def unapply(arg: Symbol): Option[String] = Some(arg.value)
  }

  case class PlainSymbol(value: String) extends Symbol

  case class GraveAccentSymbol(value: String) extends Symbol

  import Rational.Implicits._

  import Numeric.Implicits._
  import Ordering.Implicits._

  type LisaInteger = BigInt
  val LisaInteger = BigInt

  type LisaDecimal = BigDecimal
  val LisaDecimal = BigDecimal

  import LisaDecimal.double2bigDecimal
  import LisaInteger.int2bigInt

  class SNumber[T](val number: T)(implicit evidence: scala.math.Numeric[T])
    extends Expression with Ordered[SNumber[T]] with NoExternalDependency with LisaValue {
    override def toString: String = number.toString

    def mapTo[U: Numeric](implicit transform: T => U): SNumber[U] = SNumber(number)

    def toIntNumber: SNumber[LisaInteger] = number match {
      case _: LisaInteger => this.asInstanceOf[SNumber[LisaInteger]]
      case _ => SInteger(toRationalNumber.number.toIntegral)
    }

    def toDoubleNumber: SNumber[LisaDecimal] = number match {
      case _: LisaDecimal => this.asInstanceOf[SNumber[LisaDecimal]]
      case num: Rational[LisaInteger] => LisaDecimal(num.numerator) / LisaDecimal(num.denominator)
      case n: LisaInteger => LisaDecimal(n)
      case _ => SFloat(number.toDouble)
    }

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
      else {
        val thisDecimal = toDoubleNumber.number
        val thatDecimal = that.toDoubleNumber.number
        SNumber(thisDecimal / thatDecimal).asInstanceOf[SNumber[Any]]
      }
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
    implicit def wrapToSNumber[T: Numeric](t: T): SNumber[T] = apply(t)

    def apply[T: Numeric](number: T): SNumber[T] = number match {
      case s: LisaInteger => SInteger(s).asInstanceOf[SNumber[T]]
      case s: LisaDecimal => SFloat(s).asInstanceOf[SNumber[T]]
      case s: Rational[LisaInteger] => SRational(s).asInstanceOf[SNumber[T]]
      case s => new SNumber(s)
    }

    implicit def convertNumberTypes[T, U: Numeric](number: SNumber[T])(implicit transform: T => U): SNumber[U] =
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

  case class SInteger(value: LisaInteger) extends SNumber(value) {
    def <<(n: Int): SInteger = SInteger(value << n)
    def >>(n: Int): SInteger = SInteger(value >> n)

    @RawLisa def % (that: SInteger): SInteger = SInteger(value % that.value)
    @RawLisa def /% (that: SInteger): LisaList[SInteger] = {
      val (divider, remainder) = value /% that.value
      LisaList(List(divider, remainder).map(SInteger(_)))
    }

    def ~ = SInteger(~value)
    @RawLisa def &(that: SInteger): SInteger = SInteger(value & that.value)
    @RawLisa def |(that: SInteger): SInteger = SInteger(value | that.value)
    @RawLisa def ^(that: SInteger): SInteger = SInteger(value ^ that.value)

    def pow(that: Int): SInteger = SInteger(value pow that)

    def asByte: WrappedScalaObject[Byte] = WrappedScalaObject(value.toByte)
    def asChar: WrappedScalaObject[Char] = WrappedScalaObject(value.toChar)
    def asShort: WrappedScalaObject[Short] = WrappedScalaObject(value.toShort)
    def asLong: WrappedScalaObject[Long] = WrappedScalaObject(value.toLong)
  }

  object SInteger {
    private val integerCache = (-127 to 128).map(LisaInteger(_)).map(new SInteger(_))

    def apply(value: LisaInteger): SInteger = value match {
      case v if v >= -127 && v <= 128 => integerCache(v.toInt + 127)
      case v => new SInteger(v)
    }

    trait SIntegerIsNumeric extends Numeric[SInteger] {
      private val lisaIntegerIsNum = implicitly[Numeric[LisaInteger]]
      import scala.language.implicitConversions
      private[SInteger] implicit def lisaInt2SInt(li: LisaInteger): SInteger = SInteger(li)
      private[SInteger] implicit def sInt2lisaInt(si: SInteger): LisaInteger = si.value
      override def plus(x: SInteger, y: SInteger): SInteger = lisaIntegerIsNum.plus(x, y)
      override def minus(x: SInteger, y: SInteger): SInteger = lisaIntegerIsNum.minus(x, y)
      override def times(x: SInteger, y: SInteger): SInteger = lisaIntegerIsNum.times(x, y)
      override def negate(x: SInteger): SInteger = lisaIntegerIsNum.negate(x)
      override def fromInt(x: Int): SInteger = lisaIntegerIsNum.fromInt(x)
      override def parseString(str: String): Option[SInteger] = lisaIntegerIsNum.parseString(str).map(lisaInt2SInt)
      override def toInt(x: SInteger): Int = lisaIntegerIsNum.toInt(x)
      override def toLong(x: SInteger): Long = lisaIntegerIsNum.toLong(x)
      override def toFloat(x: SInteger): Float = lisaIntegerIsNum.toFloat(x)
      override def toDouble(x: SInteger): Double = lisaIntegerIsNum.toDouble(x)
      override def compare(x: SInteger, y: SInteger): Int = lisaIntegerIsNum.compare(x, y)
    }

    implicit object SIntegerIsIntegral extends Integral[SInteger] with SIntegerIsNumeric {
      private val lisaIntegerIsIntegral: Integral[LisaInteger] = implicitly
      override def quot(x: SInteger, y: SInteger): SInteger = lisaIntegerIsIntegral.quot(x, y)
      override def rem(x: SInteger, y: SInteger): SInteger = lisaIntegerIsIntegral.rem(x, y)
    }
  }

  case class SFloat(value: LisaDecimal) extends SNumber(value)

  case class SRational(value: Rational[LisaInteger]) extends SNumber(value)

  case class SBool(value: Boolean) extends Expression with NoExternalDependency with LisaValue {
    override def toString: String = value.toString

    override def tpe: LisaType = NameOnlyType("Boolean")
  }

  object SBool {
    def apply(value: Boolean): SBool =
      if (value) SBool.True else SBool.False

    val True: SBool = new SBool(true)
    val False: SBool = new SBool(false)
  }

  case class SString(value: String)
    extends Expression
      with NoExternalDependency
      with Ordered[SString]
      with LisaValue {
    override def toString: String = value.toString

    override def code: String = {
      import scala.reflect.runtime.universe._
      Literal(Constant(value)).toString()
    }

    override def compare(that: SString): Int = value compare that.value

    def charCodeAt(index: Int): Int = {
      value.charAt(index).toInt
    }
    def charCode: Int = charCodeAt(0)
  }

  case object NilObj extends Expression with NoExternalDependency with LisaValue with LisaListLike[Nothing] {
    override def list: List[Nothing] = Nil

    override def toString: String = "( )"

    override def tpe: LisaType = NameOnlyType("Nil")
  }

  case class WrappedScalaObject[+T](obj: T) extends Expression with NoExternalDependency with LisaValue {
    def get: T = obj

    override def toString: String = s"#Scala($obj)"

    override def tpe: LisaType =
      NameOnlyType(s"${getClass.getSimpleName}[${reflect.NameTransformer.decode(obj.getClass.getSimpleName)}]")

    override def equals(obj: Any): Boolean = super.equals(obj) || this.obj == obj
  }

  object WrappedScalaObject {
    def apply[T](obj: T): WrappedScalaObject[T] = obj match {
      case scala.util.Failure(ex) =>
        throw new IllegalArgumentException(s"Cannot wrap a failure", ex)
      case _ =>
        new WrappedScalaObject(obj)
    }
  }

  trait Procedure extends Expression

  case class PrimitiveFunction(function: List[Expression] => Expression)
    extends Procedure with DeclareArityAfter with NoExternalDependency {
    override def toString: String = s"#[Native Code]($function)"
  }

  object PrimitiveFunction {
    def withArityChecked(arity: Int)(function: PartialFunction[List[Expression], Expression]): PrimitiveFunction = {
      PrimitiveFunction({
        case xs if xs.length == arity => function(xs)
        case xs => throw new IllegalArgumentException(s"Arity Mismatch: Expected: $arity Given: ${xs.length}")
      }).withArity(arity)
    }
  }

  case class SideEffectFunction(function: (List[Expression], Environment) => (Expression, Environment))
    extends Procedure with NoExternalDependency {
    override def toString: String = "#[Native Code!]"

    def asMacro = PrimitiveMacro(function)
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
        case LisaList(Symbol("?" | "when" | "when?") :: xs :: Nil) =>
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

    override def toRawList: Expression = {
      val list = List(Symbol("lambda"), LisaList(boundVariable)) ::: (nestedExpressions :+ body)
      LisaList(list.map(_.toRawList))
    }
  }

  trait SelfReferencable {
    this: Expression =>
    @transient
    var selfReference: Expression = this

    def withSelf(self: Expression): this.type = {
      selfReference = self
      this
    }
  }

  trait OriginalEnvironment {
    @transient
    private var originalDefinedAt: WeakReference[Environment] = _

    @transient
    private var isDefinedAtAnyEnvironment = false

    def defineAtEnvironment(env: Environment): this.type = {
      originalDefinedAt = WeakReference(env)
      isDefinedAtAnyEnvironment = false
      this
    }
    def defineAtEnvironment(): this.type = {
      isDefinedAtAnyEnvironment = true
      originalDefinedAt = null
      this
    }

    def isDefinedAtEnvironment(env: Environment): Boolean = {
      if (isDefinedAtAnyEnvironment) true
      else if (originalDefinedAt eq null) false
      else originalDefinedAt.get.exists(_ eq env)
    }
  }

  case class Closure(boundVariable: List[Expression],
                     body: Expression,
                     capturedEnv: Environments.Environment,
                     sideEffects: List[Expression] = List.empty)
    extends Procedure with MayHaveArity with MayBeDefined with SelfReferencable with OriginalEnvironment {
    override def valid: Boolean = body.valid

    override def toString: String = s"#Closure[${genHead(boundVariable)}]"

    override def docString: String = s"${genHead(boundVariable)}: ${if (document.isEmpty) code else document}"

    def copy(boundVariable: List[Expression] = boundVariable,
             body: Expression = body,
             capturedEnv: Environments.Environment = capturedEnv,
             sideEffects: List[Expression] = sideEffects): Closure =
      Closure(boundVariable, body, capturedEnv, sideEffects).withDocString(document)

    private lazy val rawLambdaExpression = LambdaExpression(body, boundVariable, sideEffects)

    override def code: String = rawLambdaExpression.code

    override lazy val arity: Option[Int] = getArityOfPattern(boundVariable)

    override def pattern: List[List[Expression]] = boundVariable :: Nil

    override def isDefinedAt(input: List[Expression], _env: Environment): Boolean =
      super.isDefinedAt(input, capturedEnv)

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      rawLambdaExpression collectEnvDependency defined

    def flattenCaptured: Closure = copy(capturedEnv = capturedEnv.collectValues(freeVariables.toList))
  }

  case class SIfElse(predicate: Expression, consequence: Expression, alternative: Expression) extends Expression {
    override def valid: Boolean = predicate.valid && consequence.valid && alternative.valid

    override def code: String = s"(if ${predicate.code} ${consequence.code} ${alternative.code})"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      accumulateDependencies(predicate :: consequence :: alternative :: Nil, defined)

    override def toRawList: Expression =
      LisaList.fromExpression(Symbol("if"), predicate.toRawList, consequence.toRawList, alternative.toRawList)
  }

  case class SCond(conditions: List[(Expression, Expression)]) extends Expression {
    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      accumulateDependencies(conditions.flatMap(it => it._1 :: it._2 :: Nil), defined)

    override def toRawList: Expression = {
      val condition = conditions.map {
        case (SBool(true), exp) => LisaList.fromExpression(Symbol("else"), exp.toRawList)
        case (pred, consequence) => LisaList.fromExpression(pred.toRawList, consequence.toRawList)
      }
      LisaList(Symbol("cond") :: condition)
    }
  }

  case class Apply(head: Expression, args: List[Expression]) extends Expression {
    override def valid: Boolean = head.valid && args.forall(_.valid)

    override def code: String =
      if (args.isEmpty) s"(${head.code})" else s"(${head.code} ${args.map(_.code).mkString(" ")})"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) = {
      accumulateDependencies(head :: args, defined)
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

    override def toRawList: Expression = {
      LisaList((head :: args).map(_.toRawList))
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

  case class Quote(exp: Expression, isQuasiQuote: Boolean = false) extends Expression with WithFreeValues {
    override def valid: Boolean = exp.valid

    private def quotePrefix = if (isQuasiQuote) "`'" else "'"

    override def toString: String = s"$quotePrefix${exp.toString}"

    override def code: String = s"$quotePrefix${exp.code}"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      if (isQuasiQuote) {
        val deps = exp match {
          case LisaList(ll) => ll.flatMap(_.collectEnvDependency(defined)._1)
          case e => e.collectEnvDependency(defined)._1
        }

        (deps.toSet, defined)
      } else (Set.empty, defined)
  }

  case class UnQuote(quote: Expression, splicing: Boolean = false) extends Expression with NoExternalDependency {
    override def valid: Boolean = quote.valid

    private def unquotePrefix = if (splicing) "~..." else "~"

    override def toString: String = s"$unquotePrefix$quote"

    override def code: String = s"$unquotePrefix${quote.code}"

    override def collectEnvDependency(defined: Set[String]): (Set[String], Set[String]) =
      quote.collectEnvDependency(defined)
  }

  def genHead(ex: Seq[Expression]): String = {
    if (ex.isEmpty) "()"
    else ex.last match {
      case LisaList(Symbol("?" | "when") :: xs :: Nil) => s"${genHead(ex.init)} when ${xs.code}"
      case LisaList(Symbol("when?") :: xs :: Nil) => s"${genHead(ex.init)} when? ${xs.code}"
      case LisaList(Symbol("...") :: Symbol(x) :: Nil) => genHead(ex.init appended Symbol(s"...$x"))
      case _ => s"(${ex.map(_.code).mkString(" ")})"
    }
  }

  def getBoundVariables(pat: List[Expression]): Set[String] = pat match {
    case Nil => Set.empty
    case LisaList(Symbol("...") :: Symbol(sym) :: Nil) :: Nil => Set(sym)
    case LisaList(Symbol("?" | "when?" | "when") :: _) :: Nil => Set.empty
    case Symbol("_") :: xs => getBoundVariables(xs)
    case Symbol(sym) :: xs => getBoundVariables(xs) + sym
    case LisaList(Symbol("seq") :: args) :: xs => getBoundVariables(xs) ++ getBoundVariables(args)
    case LisaList(ex :: args) :: xs => getBoundVariables(ex :: xs) ++ getBoundVariables(args)
    case _ :: xs => getBoundVariables(xs)
  }

  @tailrec
  def getArityOfPattern(pat: Seq[Expression], accumulator: Int = 0): Option[Int] = pat.toList match {
    case Nil => Some(accumulator)
    case LisaList(Symbol("...") :: _) :: Nil => None // Can not count arity on va-args.
    case Symbol(s"...$_") :: Nil => None
    case LisaList(Symbol("?" | "when?" | "when") :: _) :: Nil => Some(accumulator) // Match guards
    case _ :: xs => getArityOfPattern(xs, accumulator + 1)
  }

  trait MayHaveArity {
    def arity: Option[Int]
  }

  trait DeclareArityAfter extends MayHaveArity {
    private[this] var _arity: Option[Int] = None

    override def arity: Option[Int] = _arity

    def withArity(n: Int): this.type = {
      _arity = Some(n)
      this
    }
  }

  case class SimpleMacro(paramsPattern: Seq[Expression],
                         body: Expression,
                         defines: Seq[Expression])
    extends Procedure with MayHaveArity with MayBeDefined with NoExternalDependency {
    override def valid: Boolean = paramsPattern.forall(_.valid) && body.valid && defines.forall(_.valid)

    override def toString: String = s"#Macro(${paramsPattern.mkString(" ")})"

    override def code: String = LambdaExpression(body, paramsPattern.toList, defines.toList).code

    override lazy val arity: Option[Int] = getArityOfPattern(paramsPattern)

    override def pattern: List[List[Expression]] = paramsPattern.toList :: Nil
  }

  case class SimpleMacroClosure(paramsPattern: Seq[Expression],
                                body: Expression,
                                defines: Seq[Expression],
                                capturedEnv: Environment)
  extends Procedure with MayHaveArity with MayBeDefined with NoExternalDependency with OriginalEnvironment {
    private lazy val relatingMacro = SimpleMacro(paramsPattern, body, defines)
    override lazy val arity: Option[Int] = relatingMacro.arity

    override def valid: Boolean = relatingMacro.valid

    override def code: String = relatingMacro.code

    override def pattern: List[List[Expression]] = relatingMacro.pattern

    override def toString: String = relatingMacro.toString
  }

  object SimpleMacroClosure {
    def fromMacro(m: SimpleMacro, inEnv: Environment): SimpleMacroClosure = {
      val closure = SimpleMacroClosure(m.paramsPattern, m.body, m.defines, inEnv)
      closure.copyDocString(m)
    }
  }

  case class PrimitiveMacro(fn: (List[Expression], Environment) => (Expression, Environment))
    extends Procedure with DeclareArityAfter with NoExternalDependency with CustomHintProvider {
    override def toString: String = s"#Macro![Native Code]"

    def asProcedure: SideEffectFunction = SideEffectFunction(fn)
  }

  case class Failure(tp: String, message: String) extends Expression with NoExternalDependency {
    override def valid: Boolean = false
  }

  case class PolymorphicExpression(name: String,
                                   variants: Seq[(Expression, Seq[Expression])],
                                   innerEnvironment: MutableEnv, byName: Boolean = false)
    extends Procedure with MayHaveArity with MayBeDefined with OriginalEnvironment {
    def findMatch(args: Seq[Expression],
                  inEnv: Environment = EmptyEnv): Option[(Expression, Map[String, Expression])] = {
      @annotation.tailrec
      def find(v: List[(Expression, Seq[Expression])]): Option[(Expression, Map[String, Expression])] = v match {
        case Nil => None
        case (exp, mat) :: xs =>
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
        val nc = c.copy(capturedEnv = CombineEnv.of(innerEnvironment, capturedEnv))
//          if (c.freeVariables.contains(name)) c.copy(capturedEnv = CombineEnv.of(innerEnvironment, capturedEnv))
//          else c
        val newPolymorphic = copy(variants = variants.appended((nc, nc.boundVariable)))
        nc.withSelf(newPolymorphic)
        innerEnvironment.addValue(name, newPolymorphic)
        newPolymorphic
    }


    def withExpression(mac: SimpleMacroClosure): PolymorphicExpression = {
      val newVariant = copy(variants = variants.appended((mac, mac.paramsPattern)))
      innerEnvironment.addValue(name, newVariant)
      newVariant
    }

    override def docString: String = {
      if (variants.length == 1) {
        variants.head._1.docString
      } else {
        val body = variants.map {
          case (p, v) => if (p.docString.nonEmpty) p.docString else s"${genHead(v)}: ${p.code}"
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
          case (exp, _) => Evaluator.applyToEither(exp, arguments).map(_.toString).toOption
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

    override def code: String = name
  }

  object PolymorphicExpression {
    def create(closure: Closure, name: String): PolymorphicExpression = {
      val sharedEnv = Environments.EmptyEnv.newMutableFrame
      val recursiveClosure = closure.copy(capturedEnv =
        Environments.CombineEnv.of(sharedEnv, closure.capturedEnv))
      val polymorphic =
        PolymorphicExpression(name, Seq((recursiveClosure, recursiveClosure.boundVariable)), sharedEnv)
      recursiveClosure.withSelf(polymorphic)
      sharedEnv.addValue(name, polymorphic)
      polymorphic
    }

    def create(mac: SimpleMacroClosure, name: String): PolymorphicExpression = {
      val sharedEnv = Environments.EmptyEnv.newMutableFrame
      val polymorphicExpression =
        PolymorphicExpression(name, Seq((mac, mac.paramsPattern)), sharedEnv, byName = true)
      sharedEnv.addValue(name, polymorphicExpression)
      polymorphicExpression
    }
  }

  trait LisaRecord[+V <: Expression] extends Record[String, V] with Expression with NoExternalDependency {
    def apply(sym: Symbol) = selectDynamic(sym.value)
  }
  trait LisaMutableRecord[+V <: Expression] extends LisaRecord[V] {
    def updateValue(key: String, value: Expression): Unit
  }

  object LisaRecord {
    def recordMaker(rec: List[Expression], name: String = "") = {
      @scala.annotation.tailrec
      def recurHelper(rec: List[Expression], acc: Map[String, Expression]): Map[String, Expression] = rec match {
        case Nil => acc
        case Symbol(sym) :: exp :: xs => recurHelper(xs, acc.updated(sym, exp))
        case Quote(Symbol(sym), _) :: exp :: xs => recurHelper(xs, acc.updated(sym, exp))
        case (r: LisaRecordWithMap[_]) :: xs => recurHelper(xs, acc ++ r.record)
        case x :: _ :: _ => throw new IllegalArgumentException(s"Unrecognized key type: $x: ${x.tpe.name}")
        case x :: _ => throw new IllegalArgumentException(s"No matching value for $x")
      }

      LisaMapRecord(recurHelper(rec, Map.empty), name)
    }

    def recordUpdater(rec: LisaRecordWithMap[Expression], arguments: List[Expression]) = {
      type V = Expression

      @annotation.tailrec
      def update(args: List[Expression], acc: LisaRecordWithMap[V]): LisaRecordWithMap[V] = args match {
        case Nil => acc
        case Symbol(sym) :: exp :: xs => update(xs, acc.updated(sym, exp))
        case Quote(Symbol(sym), _) :: exp :: xs => update(xs, acc.updated(sym, exp))
        case SString(sym) :: exp :: xs => update(xs, acc.updated(sym, exp))
        case (r: LisaRecordWithMap[_]) :: xs =>
          update(xs, r.record.foldLeft(acc) { case (acc, (k, v)) => acc.updated(k, v) })
        case x :: _ :: _ => throw new IllegalArgumentException(s"Unrecognized key type: $x: ${x.tpe.name}")
        case x :: _ => throw new IllegalArgumentException(s"No matching value for $x")
      }

      update(arguments, rec)
    }

    def withDefault[T <: Expression, Exp >: T <: Expression](record: LisaRecord[T], default: Exp): LisaRecord[Exp] =
      new LisaRecord[Exp] {
        override def selectDynamic(key: String): Exp = getOrElse(key, default)

        override def containsKey(key: String): Boolean = record containsKey key

        override def getOrElse[EV >: Exp](key: String, otherwise: => EV): EV = record.getOrElse(key, otherwise)

        override def recordTypeName: String = record.recordTypeName

        override def indented(bySpace: Int, level: Int): String = record.indented(bySpace, level)
      }

    def getKeyStringOf(keyLike: Expression): String = keyLike match {
      case SString(k) => k
      case Symbol(sym) => sym
      case Quote(Symbol(sym), _) => sym
      case x => throw new IllegalArgumentException(s"Unrecognized key $x.")
    }

    lazy val RecordHelperEnv = Environments.Env(Map(
      "record" -> PrimitiveFunction {
        case SString(name) :: xs =>
          recordMaker(xs, name)
        case xs =>
          recordMaker(xs)
      },
      "mutable-record" -> PrimitiveFunction {
        case Nil => MutableLisaMapRecord.empty
        case SString(name) :: Nil => MutableLisaMapRecord.empty(name)
        case (r: LisaRecordWithMap[_]) :: Nil => MutableLisaMapRecord.fromMap(r.record, r.recordTypeName)
      },
      "record-updated" -> PrimitiveFunction {
        case (r: LisaRecordWithMap[_]) :: xs => recordUpdater(r, xs)
        case _ =>
          throw new IllegalArgumentException(s"Expected a record")
      },
      "record-update!" -> PrimitiveFunction.withArityChecked(3) {
        case (r: LisaMutableRecord[_]) :: key :: value :: Nil =>
          val k = getKeyStringOf(key)
          r.updateValue(k, value)
          NilObj
      },
      "record-contains?" -> PrimitiveFunction.withArityChecked(2) {
        case (r: LisaRecord[_]) :: key :: Nil =>
          SBool(r.containsKey(getKeyStringOf(key)))
      },
      "get-record-or-else" -> PrimitiveFunction {
        case (r: LisaRecord[_]) :: SString(key) :: alter :: Nil =>
          r.getOrElse(key, alter)
        case (r: LisaRecord[_]) :: Symbol(key) :: alter :: Nil =>
          r.getOrElse(key, alter)
        case (r: LisaRecord[_]) :: Quote(Symbol(key), _) :: alter :: Nil =>
          r.getOrElse(key, alter)
        case _ =>
          throw new IllegalArgumentException()
      }.withArity(3),
      "define-record" -> PrimitiveMacro {
        case (Symbol(recordName) :: xs, env) =>
          import Implicits._
          val members = xs.map(_.asInstanceOf[Symbol])
          val membersString = members.map(_.value)
          val recordType = NameOnlyType(recordName)
          val constructor = PrimitiveFunction {
            case ls if ls.length == members.length =>
              TypedLisaRecord(membersString.zip(ls).toMap, recordType)
          }.withArity(members.length)
            .withDocString(s"Constructor for $recordName (${membersString.mkString(" ")})")
          val tester = PrimitiveFunction {
            case TypedLisaRecord(_, tpe) :: Nil if tpe eq recordType => true
            case _ :: Nil => false
            case _ => throw new IllegalArgumentException(s"Only accept on argument.")
          }.withArity(1).withDocString(s"Test if an object is constructed by $recordName")

          val duckTypeTester = PrimitiveFunction {
            case (r: LisaRecord[_]) :: Nil =>
              membersString.forall(r.containsKey)
            case _ :: Nil => false
            case _ => throw new IllegalArgumentException(s"Only accept on argument.")
          }.withArity(1).withDocString(s"Test if an object is structural identical to $recordName")

          NilObj -> env.withValues(
            Seq(recordName -> constructor, s"$recordName?" -> tester, s"$recordName??" -> duckTypeTester)
          )
        case _ =>
          throw new RuntimeException("Can not define a record like that.")
      },
      "record-with-default" -> PrimitiveFunction {
        case (r: LisaRecord[_]) :: default :: Nil =>
          LisaRecord.withDefault(r, default)
        case (r: LisaRecord[_]) :: Nil =>
          LisaRecord.withDefault(r, NilObj)
        case _ =>
          Failure("Arity Error", "record-with-default only accepts one or two arguments, first argument must me a record")
      }
    ), EmptyEnv)
  }

  abstract class AbstractLisaRecord extends LisaRecord[Expression]

  trait LisaRecordWithMap[+V <: Expression] extends LisaRecord[V] with MapRecord[String, V] {
    override def toString: String = {
      val body = record.map {
        case (k, v) => s"'$k $v"
      }.mkString(" ")
      s"$recordTypeName {$body}".stripLeading
    }

    def updated[B >: V <: Expression](key: String, value: B): LisaRecordWithMap[B]
    @RawLisa def updated[B >: V <: Expression](key: SString, value: B): LisaRecordWithMap[B] = updated(key.value, value)
  }

  case class LisaMapRecord[+V <: Expression](record: Map[String, V], recordTypeName: String = "")
    extends LisaRecordWithMap[V] {
    override def updated[B >: V <: Expression](key: String, value: B): LisaMapRecord[B] = copy(record.updated(key, value))
  }

  case class MutableLisaMapRecord(map: mutable.Map[String, Expression], recordTypeName: String = "")
    extends LisaRecordWithMap[Expression] with LisaMutableRecord[Expression] {
    def update(key: String, value: Expression): MutableLisaMapRecord = {
      map.update(key, value)
      this
    }

    override def updateValue(key: String, value: Expression): Unit = update(key, value)
    @`inline` def updateDynamic(key: String)(value: Expression): Unit = {
      update(key, value)
    }
    @RawLisa def update(key: SString, value: Expression): MutableLisaMapRecord = update(key.value, value)
    override def record: Map[String, Expression] = map.toMap
    @RawLisa override def updated[B >: Expression <: Expression](key: String, value: B): LisaRecordWithMap[B] = {
      MutableLisaMapRecord.fromMap(map.toMap.updated(key, value), recordTypeName)
    }
    def copied: MutableLisaMapRecord = copy(map.clone())
    def delete(key: String): Expression = {
      map.remove(key).getOrElse(NilObj)
    }

    override def containsKey(key: String): Boolean = map.contains(key)
    override def getOrElse[EV >: Expression](key: String, otherwise: => EV): EV = map.getOrElse(key, otherwise)

    override def toString: String = {
      val visited = mutable.ArrayBuffer.empty[Expression]
      def traverse(curr: Expression): String = curr match {
        case r @ MutableLisaMapRecord(m, name) =>
          if (visited exists r.eq) "{...}" else {
            visited.addOne(r)
            m.map {
              case (k, v) => s"'$k ${traverse(v)}"
            }.mkString(s"$name {", " ", "}").stripLeading()
          }
        case o => o.toString
      }
      traverse(this)
    }
  }
  object MutableLisaMapRecord {
    def apply(map: mutable.Map[String, Expression], recordTypeName: String = ""): MutableLisaMapRecord =
      new MutableLisaMapRecord(map, recordTypeName)
    def fromMap(map: Map[String, Expression], recordTypeName: String = ""): MutableLisaMapRecord =
      new MutableLisaMapRecord(mutable.Map.from(map), recordTypeName)
    def empty(name: String): MutableLisaMapRecord = apply(mutable.Map.empty, name)
    def empty: MutableLisaMapRecord = empty("")
  }

  case class TypedLisaRecord[+V <: Expression](record: Map[String, V], recordType: LisaType)
    extends LisaRecordWithMap[V] {
    override def recordTypeName: String = recordType.name

    override def updated[B >: V <: Expression](key: String, value: B): TypedLisaRecord[B] =
      if (record.contains(key)) copy(record.updated(key, value))
      else throw new IllegalArgumentException(s"Attribute $key does not exist on type $recordTypeName")

    override def tpe: LisaType = recordType
  }

  trait LisaListLike[+T]
    extends Seq[T]
      with Expression {
    def list: List[T]

    override def apply(i: Int): T = list(i)

    override def length: Int = list.length

    override def iterator: Iterator[T] = list.iterator

    def wrapped: WrappedScalaObject[List[T]] = WrappedScalaObject(list)

    def wrapToArray: WrappedScalaObject[Array[Any]] = WrappedScalaObject(toArray)

    override def equals(o: Any): Boolean = o match {
      case l: Seq[_] => l == list
      case WrappedScalaObject(l) => l == list
      case _ => super.equals(o)
    }
  }

  case class LisaList[+T <: Expression](list: List[T])
    extends LisaListLike[T] {
    override def tpe: LisaType = NameOnlyType("List")

    override def toString: String = list.mkString("(", " ", ")")

    override def code: String = s"${list.map(_.code).mkString("(", " ", ")")}"

    def toNativeArray: WrappedScalaObject[Array[Any]] =
      WrappedScalaObject(map(Reflect.ScalaBridge.toScalaNative).toArray)
  }

  object LisaList {
    private val nil: LisaList[Nothing] = LisaList(Nil)

    def fromExpression(exps: Expression*) = new LisaList(exps.toList)
    def empty[T <: Expression]: LisaList[T] = nil
    def newBuilder[A <: Expression]: mutable.Builder[A, LisaList[A]] = List.newBuilder.mapResult(apply)
  }

  case object PlaceHolder extends Expression

  trait IdenticalLisaExpression extends Expression

  case class SAtom private (value: String) extends IdenticalLisaExpression with NoExternalDependency {
    lazy val valuePart: String = if (value.matches("\\S+")) value else SString(value).code
    override def toString: String = s":$valuePart"

    override def tpe: LisaType = NameOnlyType("Atom")
  }

  object SAtom {
    private val cache = collection.mutable.Map.empty[String, SAtom]
    def apply(value: String): SAtom = cache.getOrElseUpdate(value, new SAtom(value))
  }

  case object JVMNull extends IdenticalLisaExpression with NoExternalDependency {
    document = "JVM null object"
    override def toString: String = "null"

    override def tpe: LisaType = NameOnlyType("Null")
  }

  case class LisaThunk(thunk: Procedure) extends Expression with NoExternalDependency {
    private val lazyObject = Util.Lazy.lazily {
      Evaluator.applyToEither(thunk, Nil).fold(ex => throw new RuntimeException(ex), identity)
    }

    lazy val value: Expression = lazyObject.get

    def isEvaluated: Boolean = lazyObject.isEvaluated

    override def toString: String = {
      if (isEvaluated) value.toString
      else "Thunk(<not computed>)"
    }
  }

  case class LisaMutableCell(var value: Expression)
    extends Expression with NoExternalDependency with IdenticalLisaExpression {
    def := (newValue: Expression): Unit = {
      value = newValue
    }
  }

  trait Implicits {
    import scala.language.implicitConversions

    implicit def fromInt(i: Int): SInteger = SInteger(i)
    implicit def fromBigInt(i: LisaInteger): SInteger = SInteger(i)
    implicit def fromString(s: String): SString = SString(s)
    implicit def fromSymbol(sym: scala.Symbol): Symbol = Symbol(sym.name)
    implicit def fromFloat(f: Float): SFloat = SFloat(LisaDecimal(f))
    implicit def fromDouble(f: Double): SFloat = SFloat(LisaDecimal(f))
    implicit def fromDecimal(f: LisaDecimal): SFloat = SFloat(f)
    implicit def fromBool(b: Boolean): SBool = SBool(b)
    implicit def autoUnit(unit: Unit): NilObj.type = NilObj
  }

  object Implicits extends Implicits

}
