package moe.roselia.lisa.Actors

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv}
import moe.roselia.lisa.{Environments, Evaluator}
import moe.roselia.lisa.Evaluator.{EvalFailure, EvalSuccess}
import moe.roselia.lisa.LispExp.{Apply, Closure, Expression, NilObj, PolymorphicExpression, PrimitiveFunction, SString, WrappedScalaObject}
import moe.roselia.lisa.Util.SymGenerator

case class PolymorphicActor private (expression: PolymorphicExpression)(context: ActorContext[Expression])
  extends AbstractBehavior[Expression](context) {
  def send(ex: Expression, to: ActorRef[Expression]): Unit
    = to ! ex
  lazy val spawnCounter = new SymGenerator
  private val sendingContext = {
    Map(
      "self" -> WrappedScalaObject(context.self),
      "!" -> PrimitiveFunction.withArityChecked(2) {
        case WrappedScalaObject(ac: ActorRef[Expression]) :: ex :: Nil =>
          send(ex, ac)
          NilObj
      },
      "same-behavior" -> WrappedScalaObject(Behaviors.same[Expression]),
      "stopped" -> WrappedScalaObject(Behaviors.stopped[Expression]),
      "spawn" -> PrimitiveFunction { arguments =>
        require(arguments.length == 2 || arguments.length == 1, "spawn receives 1 or two arguments")
        def spawn(expression: Expression, name: String) =
          WrappedScalaObject(context.spawn(
            PolymorphicActor
              .tryBuildBehavior(expression)
              .getOrElse(throw new IllegalArgumentException(s"Unexpected $expression when spawning $name.")),
            name
          ))
        arguments match {
          case ex :: Nil => spawn(ex, s"${pe.name}_Child${spawnCounter.nextCount}")
          case SString(name) :: ex :: Nil => spawn(ex, name)
        }
      }
    )
  }

  lazy val pe: PolymorphicExpression = {
    val innerEnv = expression.innerEnvironment
    sendingContext.foreach(Function.tupled(innerEnv.addValue))
    val modifiedVariants = expression.variants.map { case (closure: Closure, pattern) =>
      closure.copy(capturedEnv = CombineEnv.of(innerEnv, closure.capturedEnv)) -> pattern
    }
    expression.copy(variants = modifiedVariants, innerEnvironment = innerEnv)
  }

  override def onMessage(msg: Expression): Behavior[Expression] = {
    pe.findMatch(msg :: Nil).map[Behavior[Expression]] { case (variant, _) =>
      Evaluator.eval(Apply(variant, msg :: Nil), EmptyEnv) match {
        case EvalSuccess(WrappedScalaObject(behavior: Behavior[_]), _) =>
          behavior.asInstanceOf[Behavior[Expression]]
        case EvalSuccess(expression, env) =>
          PolymorphicActor.tryBuildBehavior(expression).getOrElse(Behaviors.stopped)
        case EvalFailure(message, jvmTrace, lisaTrace) =>
          Behaviors.stopped
      }
    }.getOrElse(Behaviors.same)
  }
}

object PolymorphicActor {
  def tryBuildBehavior(ex: Expression): Option[Behavior[Expression]] = {
    val pe = ex match {
      case cl: Closure =>
        Some(PolymorphicExpression.create(cl, ""))
      case pe: PolymorphicExpression if !pe.byName =>
        Some(pe)
      case _ => None
    }
    pe.map(apply)
  }

  def buildBehavior(expression: Expression) =
    tryBuildBehavior(expression)
      .getOrElse(throw new IllegalArgumentException(s"Unexpected $expression on building behavior."))

  def apply(pe: PolymorphicExpression): Behavior[Expression] =
    Behaviors.setup[Expression](ctx => new PolymorphicActor(pe)(ctx))
}
