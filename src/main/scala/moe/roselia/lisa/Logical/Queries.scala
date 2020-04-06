package moe.roselia.lisa.Logical

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Env, Environment, MutableEnv}
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.Evaluator.{EvalFailure, EvalSuccess}
import moe.roselia.lisa.LispExp.{Expression, LisaList, LisaMapRecord, Quote, SAtom, SBool, Symbol}


trait Queries {
  type QuerySeq[+T] = LazyList[T]
  type OutputType = QuerySeq[Environment]
  type MatcherFunction = (QuerySeq[Environment], LogicalContext) => OutputType

  val QuerySeq = LazyList
  def concatQuerySeq[T, U >: T](first: QuerySeq[T], second: => QuerySeq[U]): QuerySeq[U] =
    first #::: second
  def emptyResult: OutputType = QuerySeq.empty
  def makeResult(xs: Environment*): OutputType = QuerySeq.from(xs)

  trait Matcher extends MatcherFunction {
    def and(that: Matcher): Matcher = (in, lc) => {
      val thisOut = this(in, lc)
      that(thisOut, lc)
    }

    def or(that: Matcher): Matcher = (in, lc) => {
      val thisOut = this(in, lc)
      thisOut #::: that(in, lc)
    }

    def not: Matcher = (in, lc) => {
      @scala.annotation.tailrec
      def existPart(e: Environment, p: Environment => Boolean): Boolean = e match {
        case EmptyEnv => p(e)
        case env@Env(_, parent) => p(env) || existPart(parent, p)
        case mEnv@MutableEnv(_, parent) => p(mEnv) || existPart(parent, p)
        case env@CombineEnv(envs) =>
          if (envs.isEmpty) p(env) else p(env) || p(envs.head) || existPart(CombineEnv(envs.tail), p)
        case _ => p(e)
      }
      val thisOut = this(in, lc)
      in.filterNot(m => thisOut.exists(existPart(_, m.eq)))
    }

    def unique: Matcher = (in, lc) => {
      in.flatMap { frame =>
        val output = this(makeResult(frame), lc)
        if (output.nonEmpty && output.tail.isEmpty) output.headOption
        else emptyResult
      }
    }

    def mapEveryFrame(mapper: Environment => Environment): Matcher = and { (in, _) =>
      in.map(mapper)
    }

    def flatMapEveryFrame(mapper: Environment => IterableOnce[Environment]): Matcher = and((in, _) => in.flatMap(mapper))
  }
  object Matcher {
    def unit(e: Environment): Matcher = (_, _) => makeResult(e)
    def success: Matcher = (in, _) => in
    def fail: Matcher = (_, _) => emptyResult

    def and(matchers: Seq[Matcher]): Matcher =
      matchers.fold(success)(_ and _)
    def or(matchers: Seq[Matcher]): Matcher =
      matchers.fold(fail)(_ or _)

    def fromExpression(expression: Expression): Matcher = (in, lc) => {
      in.flatMap(constraint => {
        lc.facts.flatMap(unifyMatch(_, expression, constraint.newMutableFrame)).map(_.frozen)
      })
    }

    def fromMatchingRelationship(relationName: String, params: List[Expression], capturedEnv: Environment): Matcher = (in, lc) => {
      val matchedFromFacts = fromExpression(LisaList(Symbol(relationName) :: params))(in, lc)
      val matchedFromRules = QuerySeq.from(lc.rules).flatMap { case (ruleName, rule) =>
        fromRule(rule, params, capturedEnv)(in, lc).map(_.withValue(relationName, SAtom(ruleName)))
      }
      concatQuerySeq(matchedFromFacts, matchedFromRules)
    }

    def createUnifier(left: Expression, right: Expression): Matcher = (in, _) => {
      in.flatMap { constraint =>
        unifyMatch(left, right, constraint.newMutableFrame).map(_.frozen)
      }
    }

    def fromLisa(expression: Expression, capturedEnv: Environment): Matcher = (in, _) => {
      in.filter { env =>
        Evaluator.eval(Evaluator.unQuoteList(expression), CombineEnv(env :: capturedEnv :: Nil)) match {
          case EvalSuccess(SBool(value), _) => value
          case EvalSuccess(expression, _) =>
            throw new IllegalStateException(s"Expect a boolean, but $expression: ${expression.tpe.name} found.")
          case f@EvalFailure(_, _, _) =>
            throw new RuntimeException(f.toString)
        }
      }
    }

    def lisaExecutor(expression: Expression, capturedEnv: Environment): Matcher = (in, _) => {
      in.foreach { env =>
        Evaluator.eval(Evaluator.unQuoteList(expression), CombineEnv(env :: capturedEnv :: Nil))
      }
      in
    }

    def fromRule(rule: LogicalRule, params: List[Expression], capturedEnv: Environment): Matcher = {
      def envExtenderWithMap(map: Map[String, Expression])(env: Environment): Option[Environment] = {
        @scala.annotation.tailrec
        def traverse(m: collection.View[(String, Expression)], acc: Environment): Option[Environment] = {
          if (m.isEmpty) Some(acc)
          else m.head match { case (k, v) =>
            acc.getValueOption(k) match {
              case Some(`v`) => traverse(m.tail, acc)
              case Some(_) => None
              case None => traverse(m.tail, acc.withValue(k, v))
            }
          }
        }
        traverse(map.view, env)
      }
      Matcher.or(rule.findMatch(params).map[Matcher] { case ((matched, introduced), body) =>
        body match {
          case SBool(true) =>
            success.flatMapEveryFrame(envExtenderWithMap(introduced))
          case _ => (in, lc) => {
            val matcher =
              compileExpressionToMatcher(body, lc, capturedEnv)
            in.flatMap(envExtenderWithMap(introduced)).flatMap { previous =>
              val (undetermined, determined) = matched.partition {
                case (_, Symbol(s)) => previous.getValueOption(s).forall(_.isInstanceOf[Symbol])
                case _ => false
              }
              val determinedSubstituted = determined.view.mapValues {
                case Symbol(s) => previous.getValueOption(s).get
                case x => x
              }.toMap
              val inputs = makeResult(MutableEnv.fromMap(determinedSubstituted))

              matcher(inputs, lc).map { m =>
                undetermined.foldLeft(previous) {
                  case (e, (from, Symbol(to))) => m.getValueOption(from) match {
                    case Some(v) => e.withValue(to, v)
                    case None => e
                  }
                  case _ => previous
                }
              }
            }
          }
        }
      })
    }

    def createAssigner(value: String, expression: Expression, environment: Environment): Matcher = (in, _) => {
      in.flatMap { parentEnv =>
        Evaluator.eval(Evaluator.unQuoteList(expression), CombineEnv(Seq(parentEnv, environment))) match {
          case EvalSuccess(result, _) => parentEnv.getValueOption(value) match {
            case Some(`result`) => Some(parentEnv)
            case Some(_) => None
            case None => Some(parentEnv.withValue(value, result))
          }
          case _ => None
        }
      }
    }

    def runMatcher(matcher: Matcher)(implicit context: LogicalContext): OutputType = {
      matcher(makeResult(MutableEnv.createEmpty), context)
    }
  }

  def matchResultToLisaNative(result: OutputType): Expression = LisaList {
    result.map { e =>
      LisaMapRecord(e.flattenToMap)
    }.toList
  }

  def hasUndeterminedValue(value: Expression): Boolean = value match {
    case Symbol(_) => true
    case LisaList(ll) => ll.exists(hasUndeterminedValue)
    case _ => false
  }

  def unwrapOption[T](seq: Seq[Option[T]]): Option[Seq[T]] = {
    @annotation.tailrec
    def iter(s: Seq[Option[T]], acc: Seq[T]): Option[Seq[T]] = {
      if (s.isEmpty) Some(acc)
      else s.head match {
        case Some(x) => iter(s.tail, x +: acc)
        case None => None
      }
    }
    iter(seq, Seq.empty)
  }

  def trySubstituteValue(expression: Expression, context: Environment): Option[Expression] = expression match {
    case Symbol(sym) => context.getValueOption(sym).flatMap(trySubstituteValue(_, context))
    case LisaList(ll) => unwrapOption(ll.map(trySubstituteValue(_, context))).map(_.toList).map(LisaList(_))
    case x => Some(x)
  }

  def substituteAllPossibilities(expression: Expression, context: Environment): Expression = expression match {
    case s @ Symbol(symbol) => context.getValueOption(symbol).getOrElse(s)
    case LisaList(ll) => LisaList(ll.map(substituteAllPossibilities(_, context)))
    case x => x
  }

  def replacePreservingHeads(expression: Expression,
                             replaceBy: PartialFunction[Expression, Expression]): Expression = expression match {
    case LisaList(head :: tail) => LisaList(head :: tail.map(replacePreservingHeads(_, replaceBy)))
    case x => replaceBy.applyOrElse(x, identity[Expression])
  }

  def replaceIfPossible(expression: Expression,
                             replaceBy: PartialFunction[Expression, Expression]): Expression = expression match {
    case LisaList(ll) => LisaList(ll.map(replacePreservingHeads(_, replaceBy)))
    case x => replaceBy.applyOrElse(x, identity[Expression])
  }

  def dependsOn(expression: Expression, variable: String, constraint: Environment): Boolean = expression match {
    case Symbol(`variable`) => true
    case Symbol(symbol) => constraint.getValueOption(symbol).exists(dependsOn(_, variable, constraint))
    case LisaList(ll) => ll.exists(dependsOn(_, variable, constraint))
    case _ => false
  }

  def extendUndeterminedIfPossible(name: String, exp: Expression, environment: MutableEnv): Option[MutableEnv] = {
    def extendList(env: Option[MutableEnv]) = {
      env.foreach { e =>
        e.addValue(name, substituteAllPossibilities(exp, e))
      }
      env
    }
    // Ignore match
    if(name == "_") Some(environment)
    else environment.getValueOption(name) match {
      case Some(x) => extendList(unifyMatch(x, exp, environment))
      case _ => exp match {
        case Symbol(sym) => environment.getValueOption(sym) match {
          case Some(value) => extendList(unifyMatch(Symbol(name), value, environment))
          case _ =>
            environment.addValue(name, exp)
            Some(environment)
        }
        case _ =>
          if (dependsOn(exp, name, environment)) None
          else Some(environment.addValue(name, exp))
      }
    }
  }

  def matchPatternOfList(pattern: List[Expression],
                             data: List[Expression],
                             constraints: MutableEnv = MutableEnv.createEmpty): Option[MutableEnv] = pattern match {
    case Nil => if (data.isEmpty) Some(constraints) else None

    case LisaList(Symbol("...") :: (sym@Symbol(_)) :: Nil) :: xs =>
      if (xs.isEmpty) {
        unifyMatch(sym, LisaList(data), constraints)
      }
      else None
    case otherwise :: xs => data match {
      case `otherwise` :: ys => matchPatternOfList(xs, ys, constraints)
      case LisaList(Symbol("...") :: (sym@Symbol(_)) :: Nil) :: xs =>
        if (xs.isEmpty) unifyMatch(LisaList(pattern), sym, constraints)
        else None
      case y :: ys => for {
        _ <- unifyMatch(otherwise, y, constraints)
        tailMatch <- matchPatternOfList(xs, ys, constraints)
      } yield tailMatch
      case _ => None
    }
  }

  def unifyMatch(pattern1: Expression, pattern2: Expression, constraints: MutableEnv): Option[MutableEnv] = {
//    println(s"Match $pattern1 and $pattern2 with $constraints")
    if(pattern1 == pattern2) Some(constraints)
    else pattern1 match {
      case Symbol(name) => extendUndeterminedIfPossible(name, pattern2, constraints)
      case _ => pattern2 match {
        case Symbol(name) => extendUndeterminedIfPossible(name, pattern1, constraints)
        case LisaList(ll2) => pattern1 match {
          case LisaList(ll1) => matchPatternOfList(ll1, ll2, constraints)
          case _ => None
        }
        case _ => None
      }
    }
  }

  def compileExpressionToMatcher(expression: Expression,
                                 context: LogicalContext,
                                 inEnv: Environment): Matcher = {
    @`inline` def compile(exp: Expression): Matcher = compileExpressionToMatcher(exp, context, inEnv)
    expression match {
      case LisaList(ll) => ll match {
        case Symbol("and") :: xs => Matcher.and(xs.map(compile))
        case Symbol("or") :: xs => Matcher.or(xs.map(compile))
        case Symbol("not") :: x :: Nil => compile(x).not
        case Symbol("?") :: x :: Nil => Matcher.fromLisa(x, inEnv)
        case Symbol("execute-lisa") :: x :: Nil => Matcher.lisaExecutor(x, inEnv)
        case Symbol("=") :: lhs :: rhs :: Nil => Matcher.createUnifier(lhs, rhs)
        case Symbol("<-") :: Symbol(name) :: exp :: Nil => Matcher.createAssigner(name, exp, inEnv)
        case Symbol("unique") :: x :: Nil => compile(x).unique
        case Symbol(sym) :: xs if context.hasRule(sym) =>
          Matcher.fromRule(context.getRule(sym).get, xs, inEnv)
        case Quote(Symbol(name)) :: xs =>
          Matcher.fromMatchingRelationship(name, xs, inEnv)
        case Symbol(sym) :: xs =>
          Matcher.fromExpression(LisaList(SAtom(sym) :: xs))
        case xs => Matcher.fromExpression(LisaList(xs))
      }
      case x => Matcher.fromExpression(x)
    }
  }
}

object Queries extends Queries
