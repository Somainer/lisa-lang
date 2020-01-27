package moe.roselia.lisa.Util

import moe.roselia.lisa.LispExp.Expression

object ReturnControlFlow {

  class ReturnFlag

  case class ReturnException[T](exp: T, returnFlag: ReturnFlag) extends scala.util.control.ControlThrowable

  class Returns[+T] {
    private[this] val returnFlag = new ReturnFlag

    def returns[U >: T](value: U) = throw ReturnException(value, returnFlag)

    def returnable[U >: T](op: => U): U = try op catch {
      case ReturnException(result: U, `returnFlag`) =>
        result
    }
  }

  object Returns extends Returns[Expression]

}

