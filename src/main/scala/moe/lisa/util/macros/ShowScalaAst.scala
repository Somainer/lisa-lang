package moe.lisa.util.macros

import scala.quoted._
import scala.annotation.targetName

object ShowScalaAst:
  private def inspectImpl[T](ex: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect._
    val term = ex.asTerm match
      case Inlined(_, _, x) => x
      case x => x
    report.info(s"${term.show} \n ${term.toString}")
    ex

  inline def inspectAst[T](inline ex: T): T = ${ inspectImpl('ex) }

  extension[T] (inline self: T)
    inline def inspect: T = ${ inspectImpl('self) }
