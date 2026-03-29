package moe.roselia.lisa.JIT

import scala.util.Try
import scala.quoted.*
import scala.quoted.staging.*

import moe.roselia.lisa.LispExp.*
import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.Evaluator

/**
 * JIT Compiler for Lisa using Scala 3's multistage programming (scala.quoted.staging).
 *
 * This compiles frequently-called Lisa closures into native JVM functions by:
 * 1. Analyzing the closure's body AST
 * 2. Generating Scala 3 Expr code via quoting
 * 3. Compiling it at runtime via staging.run
 *
 * Only simple, pure numeric/string expressions are JIT-compiled.
 * Complex closures (with side effects, macros, etc.) fall back to interpretation.
 */
object JITCompiler {

  /** Lazily initialize the staging compiler */
  private lazy val stagingCompiler: Option[staging.Compiler] =
    Try(staging.Compiler.make(getClass.getClassLoader)).toOption

  /** Attempt to JIT compile a closure. Returns None if compilation fails or is not applicable. */
  def tryCompile(closure: Closure): Option[Expression] = {
    stagingCompiler.flatMap { implicit compiler =>
      closure.boundVariable match {
        case param :: Nil =>
          tryCompileUnary(closure, param)
        case p1 :: p2 :: Nil =>
          tryCompileBinary(closure, p1, p2)
        case _ => None
      }
    }
  }

  /** Try to compile a unary closure (single parameter) */
  private def tryCompileUnary(closure: Closure, param: Expression)(implicit compiler: staging.Compiler): Option[Expression] = {
    val paramName = param match {
      case Symbol(name) => name
      case PlainSymbol(name) => name
      case _ => return None
    }

    analyzeBodyType(closure.body, Map(paramName -> ExprType.Numeric)) match {
      case Some(ExprType.Numeric) =>
        tryCompileNumericUnary(closure, paramName)
      case Some(ExprType.Boolean) =>
        tryCompilePredicateUnary(closure, paramName)
      case _ => None
    }
  }

  /** Try to compile a binary closure (two parameters) */
  private def tryCompileBinary(closure: Closure, p1: Expression, p2: Expression)(implicit compiler: staging.Compiler): Option[Expression] = {
    val name1 = symbolName(p1).getOrElse(return None)
    val name2 = symbolName(p2).getOrElse(return None)

    analyzeBodyType(closure.body, Map(name1 -> ExprType.Numeric, name2 -> ExprType.Numeric)) match {
      case Some(ExprType.Numeric) =>
        tryCompileNumericBinary(closure, name1, name2)
      case Some(ExprType.Boolean) =>
        tryCompilePredicateBinary(closure, name1, name2)
      case _ => None
    }
  }

  private def symbolName(expr: Expression): Option[String] = expr match {
    case Symbol(name) => Some(name)
    case PlainSymbol(name) => Some(name)
    case _ => None
  }

  private def toDouble(arg: SNumber[?]): Double =
    arg.toDoubleNumber.number.toDouble

  /** Compile a unary numeric function using staging */
  private def tryCompileNumericUnary(closure: Closure, paramName: String)(implicit compiler: staging.Compiler): Option[Expression] = {
    Try {
      val compiledFn: Double => Double = staging.run {
        '{ (x: Double) => ${compileNumericExpr(closure.body, Map(paramName -> 'x))} }
      }

      Some(PrimitiveFunction {
        case (arg: SNumber[?]) :: Nil =>
          val result = compiledFn(toDouble(arg))
          if (result == result.toLong.toDouble) SInteger(BigInt(result.toLong))
          else SFloat(BigDecimal(result))
        case args => Evaluator.applyToEither(closure, args).fold(
          err => Failure("JIT Fallback", err), identity
        )
      }.withDocString(s"[JIT compiled] ${closure.docString}").withArity(1))
    }.toOption.flatten
  }

  /** Compile a unary predicate function using staging */
  private def tryCompilePredicateUnary(closure: Closure, paramName: String)(implicit compiler: staging.Compiler): Option[Expression] = {
    Try {
      val compiledFn: Double => Boolean = staging.run {
        '{ (x: Double) => ${compileBoolExpr(closure.body, Map(paramName -> 'x))} }
      }

      Some(PrimitiveFunction {
        case (arg: SNumber[?]) :: Nil =>
          SBool(compiledFn(toDouble(arg)))
        case args => Evaluator.applyToEither(closure, args).fold(
          err => Failure("JIT Fallback", err), identity
        )
      }.withDocString(s"[JIT compiled] ${closure.docString}").withArity(1))
    }.toOption.flatten
  }

  /** Compile a binary numeric function using staging */
  private def tryCompileNumericBinary(closure: Closure, name1: String, name2: String)(implicit compiler: staging.Compiler): Option[Expression] = {
    Try {
      val compiledFn: (Double, Double) => Double = staging.run {
        '{ (x: Double, y: Double) => ${compileNumericExpr(closure.body, Map(name1 -> 'x, name2 -> 'y))} }
      }

      Some(PrimitiveFunction {
        case (a: SNumber[?]) :: (b: SNumber[?]) :: Nil =>
          val result = compiledFn(toDouble(a), toDouble(b))
          if (result == result.toLong.toDouble) SInteger(BigInt(result.toLong))
          else SFloat(BigDecimal(result))
        case args => Evaluator.applyToEither(closure, args).fold(
          err => Failure("JIT Fallback", err), identity
        )
      }.withDocString(s"[JIT compiled] ${closure.docString}").withArity(2))
    }.toOption.flatten
  }

  /** Compile a binary predicate function using staging */
  private def tryCompilePredicateBinary(closure: Closure, name1: String, name2: String)(implicit compiler: staging.Compiler): Option[Expression] = {
    Try {
      val compiledFn: (Double, Double) => Boolean = staging.run {
        '{ (x: Double, y: Double) => ${compileBoolExpr(closure.body, Map(name1 -> 'x, name2 -> 'y))} }
      }

      Some(PrimitiveFunction {
        case (a: SNumber[?]) :: (b: SNumber[?]) :: Nil =>
          SBool(compiledFn(toDouble(a), toDouble(b)))
        case args => Evaluator.applyToEither(closure, args).fold(
          err => Failure("JIT Fallback", err), identity
        )
      }.withDocString(s"[JIT compiled] ${closure.docString}").withArity(2))
    }.toOption.flatten
  }

  /** Compile a Lisa expression into a Scala 3 Expr[Double] using quoting */
  private def compileNumericExpr(expr: Expression, env: Map[String, Expr[Double]])(using Quotes): Expr[Double] = {
    expr match {
      case Symbol(name) if env.contains(name) => env(name)
      case PlainSymbol(name) if env.contains(name) => env(name)
      case SInteger(n) => Expr(n.toDouble)
      case SFloat(n) => Expr(n.toDouble)

      case Apply(Symbol("+"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea + $eb }

      case Apply(Symbol("-"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea - $eb }

      case Apply(Symbol("-"), List(a)) =>
        val ea = compileNumericExpr(a, env)
        '{ -$ea }

      case Apply(Symbol("*"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea * $eb }

      case Apply(Symbol("/"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea / $eb }

      case Apply(Symbol("mod"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea % $eb }

      case Apply(Symbol("abs"), List(a)) =>
        val ea = compileNumericExpr(a, env)
        '{ math.abs($ea) }

      case Apply(Symbol("max"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ math.max($ea, $eb) }

      case Apply(Symbol("min"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ math.min($ea, $eb) }

      case Apply(Symbol("if"), List(cond, thenBranch, elseBranch)) =>
        val ec = compileBoolExpr(cond, env)
        val et = compileNumericExpr(thenBranch, env)
        val ee = compileNumericExpr(elseBranch, env)
        '{ if $ec then $et else $ee }

      case _ => throw new UnsupportedOperationException(s"Cannot JIT compile: ${expr.code}")
    }
  }

  /** Compile a Lisa expression into a Scala 3 Expr[Boolean] using quoting */
  private def compileBoolExpr(expr: Expression, env: Map[String, Expr[Double]])(using Quotes): Expr[Boolean] = {
    expr match {
      case SBool(b) => Expr(b)

      case Apply(Symbol("<"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea < $eb }

      case Apply(Symbol("<="), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea <= $eb }

      case Apply(Symbol(">"), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea > $eb }

      case Apply(Symbol(">="), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea >= $eb }

      case Apply(Symbol("="), List(a, b)) =>
        val ea = compileNumericExpr(a, env)
        val eb = compileNumericExpr(b, env)
        '{ $ea == $eb }

      case Apply(Symbol("and"), args) =>
        args.map(a => compileBoolExpr(a, env)).reduce((a, b) => '{ $a && $b })

      case Apply(Symbol("or"), args) =>
        args.map(a => compileBoolExpr(a, env)).reduce((a, b) => '{ $a || $b })

      case Apply(Symbol("not"), List(a)) =>
        val ea = compileBoolExpr(a, env)
        '{ !$ea }

      case Apply(Symbol("if"), List(cond, thenBranch, elseBranch)) =>
        val ec = compileBoolExpr(cond, env)
        val et = compileBoolExpr(thenBranch, env)
        val ee = compileBoolExpr(elseBranch, env)
        '{ if $ec then $et else $ee }

      case _ => throw new UnsupportedOperationException(s"Cannot JIT compile boolean: ${expr.code}")
    }
  }

  /** Expression type categories for JIT analysis */
  private enum ExprType {
    case Numeric, Boolean, String, Unknown
  }

  /** Analyze what type a Lisa expression body would produce */
  private def analyzeBodyType(expr: Expression, paramTypes: Map[String, ExprType]): Option[ExprType] = {
    expr match {
      case _: SNumber[?] => Some(ExprType.Numeric)
      case _: SBool => Some(ExprType.Boolean)
      case _: SString => Some(ExprType.String)
      case Symbol(name) if paramTypes.contains(name) => Some(paramTypes(name))
      case PlainSymbol(name) if paramTypes.contains(name) => Some(paramTypes(name))

      case Apply(Symbol(op), _) if Set("+", "-", "*", "/", "mod", "abs", "max", "min").contains(op) =>
        Some(ExprType.Numeric)

      case Apply(Symbol(op), _) if Set("<", "<=", ">", ">=", "=", "and", "or", "not").contains(op) =>
        Some(ExprType.Boolean)

      case Apply(Symbol("if"), List(_, thenBranch, _)) =>
        analyzeBodyType(thenBranch, paramTypes)

      case _ => None
    }
  }

  /** Check if JIT compilation is available in this environment */
  def isAvailable: Boolean = stagingCompiler.isDefined
}
