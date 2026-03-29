package moe.roselia.lisa.Repl

import moe.roselia.lisa.Environments.Environment
import moe.roselia.lisa.LispExp.{CustomHintProvider, SNumber, SString, WrappedScalaObject}
import moe.roselia.lisa.Reflect.{DotAccessor, ScalaBridge, StaticFieldAccessor}
import moe.roselia.lisa.Util.SimilarSymbolFinder
import moe.roselia.lisa.{Evaluator, LispExp}

import java.lang.reflect.{Method, Modifier}

trait HintProvider:
  def nextSymbol(input: String): Option[LispExp.Symbol] =
    import moe.roselia.lisa.SExpressionParser.*
    val parsed = parse(sValue, input).map(Evaluator.compile)
    val result = if parsed.successful then Some(parsed.get) else None
    result.collect {
      case s: LispExp.Symbol => s
    }

  case class MemberInfo(name: String, typeName: String, isMethod: Boolean, paramCount: Int)

  def provideMemberSymbols(word: String, restInput: String)(implicit state: State): Seq[MemberInfo] =
    nextSymbol(restInput).map(_.value)
      .filter(state.environment.collectDefinedValues)
      .map(state.environment.get)
      .map {
        case WrappedScalaObject(obj) => obj
        case SString(value) => value
        case sn: SNumber[?] => ScalaBridge.toScalaNative(sn)
        case x => x
      }.toSeq
      .flatMap { obj =>
        val clazz = obj.getClass
        val methods = clazz.getMethods.filter(m => Modifier.isPublic(m.getModifiers))
        methods.filter(m =>
          scala.reflect.NameTransformer.decode(m.getName).toLowerCase.startsWith(word.toLowerCase)
        ).map { m =>
          val decodedName = scala.reflect.NameTransformer.decode(m.getName)
          val params = m.getParameterTypes.map(_.getSimpleName).mkString(", ")
          val retType = m.getReturnType.getSimpleName
          val typeSig = if m.getParameterCount == 0 then retType else s"($params) => $retType"
          MemberInfo(decodedName, typeSig, true, m.getParameterCount)
        }.toSeq.distinctBy(_.name)
      }

  def findSimilarSymbols(word: String, words: Seq[String]): Seq[String] =
    val threshold = (word.length * 0.2).toInt.max(4)
    val lowerWord = word.toLowerCase
    if word.length <= 1 then Nil
    else words.filter(SimilarSymbolFinder.editDistanceWeighed(_, lowerWord) <= threshold)

  def provideMacroHint(macroLike: String, word: String)(environment: Environment): Seq[(String, String)] =
    nextSymbol(macroLike)
      .map(_.value)
      .flatMap(environment.getValueOption)
      .collect {
        case x: CustomHintProvider => x
      }
      .toSeq
      .flatMap(_.provideHint(word))

object HintProvider extends HintProvider
