package moe.roselia.lisa.Library

import sys.process._
import sys.env
import util.Properties
import moe.roselia.lisa.Environments.{EmptyEnv, Env}
import moe.roselia.lisa.LispExp.{LisaMapRecord, NilObj, PrimitiveFunction, SString, WrappedScalaObject}
import moe.roselia.lisa.LispExp.Implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object System {
  lazy val systemEnv = Env(Map(
    "system" -> PrimitiveFunction {
      xs => stringSeqToProcess(xs.map(_.toString)).!!
    },
    "system/fork" -> PrimitiveFunction { xs =>
      WrappedScalaObject(Future {
        xs.map(_.toString).!!
      })
    },
    "get-system-environment" -> PrimitiveFunction {
      case SString(name) :: SString(alternative) :: Nil =>
        Properties.envOrElse(name, alternative)
      case SString(name) :: Nil =>
        Properties.envOrElse(name, throw new NoSuchElementException(s"Environment variable $name not found"))
      case _ =>
        throw new IllegalArgumentException()
    },
    "system-environment" -> LisaMapRecord(env.view.mapValues(SString).toMap, "System"),
    "get-prop" -> PrimitiveFunction {
      case SString(name) :: Nil =>
        Properties.propOrEmpty(name)
      case SString(name) :: SString(alternative) :: Nil =>
        Properties.propOrElse(name, alternative)
      case _ =>
        throw new IllegalArgumentException()
    },
    "get-scala-prop" -> PrimitiveFunction {
      case SString(name) :: Nil =>
        Properties.scalaPropOrEmpty(name)
      case SString(name) :: SString(alternative) :: Nil =>
        Properties.scalaPropOrElse(name, alternative)
      case _ =>
        throw new IllegalArgumentException()
    },
    "throw" -> PrimitiveFunction.withArityChecked(1) {
      case WrappedScalaObject(ex: Throwable) :: Nil => throw ex
      case x :: Nil => throw new IllegalArgumentException(s"$x is not throwable.")
    }
  ), EmptyEnv)
}
