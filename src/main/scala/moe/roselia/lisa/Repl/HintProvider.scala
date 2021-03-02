package moe.roselia.lisa.Repl

import moe.roselia.lisa.Environments.Environment

import scala.reflect.runtime.universe
import moe.roselia.lisa.LispExp.{CustomHintProvider, SNumber, SString, WrappedScalaObject}
import moe.roselia.lisa.Reflect.{DotAccessor, ScalaBridge, StaticFieldAccessor}
import moe.roselia.lisa.Util.SimilarSymbolFinder
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
      .filter(m => symbolName(m).toLowerCase.startsWith(word.toLowerCase))
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

  def findSimilarSymbols(word: String, words: Seq[String]): Seq[String] = {
    val threshold = (word.length * 0.2).toInt.max(4)
    val lowerWord = word.toLowerCase
    if (word.length <= 1) Nil
    else {
      words.filter(SimilarSymbolFinder.editDistanceWeighed(_, lowerWord) <= threshold)
    }
  }

  def provideMacroHint(macroLike: String, word: String)(environment: Environment): Seq[(String, String)] = {
    nextSymbol(macroLike)
      .map(_.value)
      .flatMap(environment.getValueOption)
      .collect {
        case x: CustomHintProvider => x
      }
      .toSeq
      .flatMap(_.provideHint(word))
  }
}

object HintProvider extends HintProvider
