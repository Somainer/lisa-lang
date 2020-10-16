package moe.roselia.lisa.Util

import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{Closure, Expression, Failure, LisaList, LisaListLike, PolymorphicExpression, PrimitiveFunction, Procedure, SBool, SString, WrappedScalaObject}
import moe.roselia.lisa.Reflect.ScalaBridge.fromScalaNative

object CollectionHelper {
  /**
   * Helper to deal with boxed collections and native collections
   */


  def generalMap(ls: Iterable[Any], fn: Expression): Iterable[Expression] = fn match {
    case PrimitiveFunction(fn) =>
      ls.map(x => fn(List(fromScalaNative(x))))
    case c: Procedure =>
      val newList = ls.map(x => Evaluator.applyToEither(c, List(fromScalaNative(x))))
      if(newList forall (_.isRight))
        newList.map(_.toOption.get)
      else newList.find(_.isLeft).get.left.toOption.map(err => throw new RuntimeException(s"map Error: $err"))
    case _ => throw new RuntimeException(s"map Error: Cannot map $fn on $ls")
  }

  def generalFilter(ls: Iterable[Expression], fn: Expression): Iterable[Expression] = {
    def ensureBool(e: Expression) = e match {
      case SBool(b) => b
      case _ => throw new IllegalArgumentException("Function should return a Bool.")
    }
    def reportFilterError(reason: String) = throw new RuntimeException(s"filter Error: $reason")

    fn match {
      case PrimitiveFunction(fn) =>
        ls.filter(x => ensureBool(fn(List(fromScalaNative(x)))))
      case WrappedScalaObject(obj) =>
        ls.filter(x =>
          obj.asInstanceOf[{def apply(a: Any): Boolean}].apply(x))
      case c: Procedure =>
        val newList = ls.map(x => Evaluator.applyToEither(c, List(fromScalaNative(x))))
        if(newList forall (_.isRight))
          newList.map(_.toOption.get).zip(ls).filter(x => ensureBool(x._1)).map(_._2)
        else newList.find(_.isLeft).get.left.toOption.map(reportFilterError)
      case _ => reportFilterError(s"Cannot filter $fn on $ls")
    }
  }

  def lazyConst[T, U](x: => T)(y: U): T = x

  def generalGetElementOfSeqLike(coll: Expression, index: Int, default: => Expression): Expression = coll match {
    case ll: LisaListLike[Expression] => ll.applyOrElse(index, lazyConst(default))
    case WrappedScalaObject(it: Iterable[_]) =>
      fromScalaNative(it.toSeq.applyOrElse(index, lazyConst(default)))
    case WrappedScalaObject(array: Array[_]) =>
      fromScalaNative(array.applyOrElse(index, lazyConst(default)))
    case SString(value) => SString(value.applyOrElse(index, lazyConst(default)).toString)
    case xs: Iterable[_] => fromScalaNative(xs.view.toSeq.applyOrElse(index, lazyConst(default)))
    case xs => throw new IllegalArgumentException(s"Contract violation: ${xs.tpe.name} is not seq-like.")
  }

  def generalSortWith(coll: Expression, byFn: Expression): Expression = {
    def byExpression(lhs: Expression, rhs: Expression): Boolean = Evaluator.applyToEither(byFn, lhs :: rhs :: Nil) match {
      case Left(value) => throw new IllegalStateException(s"Sort error: $value")
      case Right(SBool(value)) => value
      case Right(otherwise) => throw new IllegalArgumentException(s"Expected a boolean, but $otherwise found.")
    }

    coll match {
      case ll: LisaListLike[Expression] => LisaList(ll.sortWith(byExpression).toList)
      case WrappedScalaObject(seq: Seq[_]) =>
        fromScalaNative(seq.sortWith((lhs, rhs) => byExpression(fromScalaNative(lhs), fromScalaNative(rhs))))
      case xs => throw new IllegalArgumentException(s"Contract violation: ${xs.tpe.name} is not seq-like.")
    }
  }
}
