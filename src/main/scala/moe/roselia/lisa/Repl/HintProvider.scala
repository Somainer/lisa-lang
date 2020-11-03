package moe.roselia.lisa.Repl

import scala.reflect.runtime.universe
import moe.roselia.lisa.LispExp.{SNumber, SString, WrappedScalaObject}
import moe.roselia.lisa.Reflect.{DotAccessor, ScalaBridge, StaticFieldAccessor}
import moe.roselia.lisa.{Evaluator, LispExp}

trait HintProvider {
  def nextSymbol(input: String): Option[LispExp.Symbol] = {
    import moe.roselia.lisa.SExpressionParser._

    val parsed = parse(sValue, input).map(Evaluator.compile)
    val result = if (parsed.successful) Some(parsed.get) else None
    result.collect {
      case s: LispExp.Symbol => s
    }
  }

  def provideMemberSymbols(word: String, restInput: String)(implicit state: State): Seq[universe.Symbol] = {
    nextSymbol(restInput).map(_.value)
      .filter(state.environment.collectDefinedValues) // To prevent accidentally executing reflection.
      .map(state.environment.get)
      .map {
        case WrappedScalaObject(obj) => obj
        case SString(value) => value
        case sn: SNumber[_] => ScalaBridge.toScalaNative(sn)
        case x => x
      }.map(DotAccessor.getClassObject)
      .map(_.symbol)
      .toSeq
      .flatMap(DotAccessor.getDeclaredTerms)
      .filter(m => symbolName(m).startsWith(word))
      .flatMap(_.alternatives)
  }

  def symbolName(symbol: universe.Symbol): String = symbol.name.decodedName.toString
  @`inline` def symbolTypeName(symbol: universe.Symbol): String = symbolName(symbol.typeSignature.typeSymbol)

  def symbolToTypeSignature(symbol: universe.Symbol): String = {
    if (symbol.isMethod) {
      val method = symbol.asMethod
      val params = method.paramLists match {
        case Nil => "()"
        case param :: _ => s"(${param.map(symbolTypeName).mkString(", ")})"
      }
      val returnType = symbolName(method.returnType.typeSymbol)
      s"$params => $returnType"
    } else symbolTypeName(symbol)
  }
}

object HintProvider extends HintProvider
