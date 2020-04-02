package moe.roselia.lisa

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, SpawnProtocol}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.{Await, Awaitable, ExecutionContext, Future, Promise}
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.duration._
import moe.roselia.lisa.Environments.EmptyEnv
import moe.roselia.lisa.LispExp.{Expression, LisaList, NilObj, PrimitiveFunction, SInteger, SString, WrappedScalaObject}

package object Actors {
  private val mainBehavior: Behavior[SpawnProtocol.Command] = Behaviors.setup { _ =>
    SpawnProtocol()
  }
  private implicit val system: ActorSystem[SpawnProtocol.Command] = ActorSystem(mainBehavior, "Main")
  implicit val ec: ExecutionContext = system.executionContext
  implicit val timeout: Timeout = 3.seconds

  def spawnExpressionActorAsync(expression: Expression, name: String): Future[ActorRef[Expression]] = {
    val behavior = PolymorphicActor.buildBehavior(expression)
    system.ask(SpawnProtocol.Spawn(behavior, name, Props.empty, _))
  }
  def spawnExpressionActor(expression: LispExp.Expression, str: String): ActorRef[Expression] =
    Await.result(spawnExpressionActorAsync(expression, str), 4.seconds)

  private val actorsCount = new Util.SymGenerator
  lazy val ActorEnvironment: Environments.NameSpacedEnv = Environments.NameSpacedEnv("actors", EmptyEnv.withValues(
    Seq(
      "spawn" -> PrimitiveFunction {
        case ex :: Nil => WrappedScalaObject(spawnExpressionActor(ex, s"Main_${actorsCount.nextCount}"))
        case SString(name) :: ex :: Nil =>
          WrappedScalaObject(spawnExpressionActor(ex, name))
      },
      "!" -> PrimitiveFunction.withArityChecked(2) {
        case WrappedScalaObject(actor: ActorRef[Expression]) :: ex :: Nil =>
          actor ! ex
          NilObj
      },
      "await" -> PrimitiveFunction {
        case WrappedScalaObject(await: Awaitable[_]) :: SInteger(timeoutMs) :: Nil =>
          Reflect.ScalaBridge.fromScalaNative(Await.result(await, timeoutMs.toInt.microseconds))
        case WrappedScalaObject(await: Awaitable[_]) :: Nil =>
          Reflect.ScalaBridge.fromScalaNative(Await.result(await, Duration.Inf))
      },
      "ask" -> PrimitiveFunction.withArityChecked(2) {
        case WrappedScalaObject(actorRef: ActorRef[Expression]) :: question :: Nil =>
          WrappedScalaObject(actorRef.ask[Expression](ref => LisaList(question :: WrappedScalaObject(ref) :: Nil)))
      },
      "future" -> PrimitiveFunction.withArityChecked(1) {
        case fn :: Nil => WrappedScalaObject(Future {
          Evaluator.applyToEither(fn, Nil) match {
            case Right(value) => value
            case Left(ex) => throw new RuntimeException(ex)
          }
        })
      },
      "promise" -> PrimitiveFunction.withArityChecked(1) {
        case fn :: Nil =>
          val promise = Promise[Expression]()
          val success = PrimitiveFunction.withArityChecked(1) { case value :: Nil =>
            promise.success(value)
            NilObj
          }
          val failure = PrimitiveFunction.withArityChecked(1) {
            case WrappedScalaObject(ex: Throwable) :: Nil =>
              promise.failure(ex)
              NilObj
            case _ => throw new IllegalArgumentException(s"Failure must be an exception.")
          }
          Evaluator.applyToEither(fn, success :: failure :: Nil).left.foreach(s => promise.failure(new RuntimeException(s)))
          WrappedScalaObject(promise)
      }
    )
  ), "/")
}
