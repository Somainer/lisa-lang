package moe.roselia.lisa

import moe.roselia.lisa.LispExp.{Apply, LambdaExpression, LisaList, PlainSymbol, Symbol}
import moe.roselia.lisa.SimpleLispTree._

import scala.util.matching.Regex
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}

object SExpressionParser extends ImplicitConversions with RegexParsers {
  override protected val whiteSpace = """(\s|;.*)+""".r

  def locational[T <: Locational](p: Parser[T]): Parser[T] = Parser { in =>
    val offset = in.offset
    val start = handleWhiteSpace(in.source, offset)
    p(in.drop(start - offset)) match {
      case Success(result, next) =>
        Success(result.setLocation(Location(
          in.source,
          start, next.offset
        )), next)
      case ns => ns
    }
  }

  def sValue: SExpressionParser.Parser[Value] = sValueExclude("")
  def sValueExclude(ex: String): SExpressionParser.Parser[Value] =
    ("`.+`".r.map(_.drop(1).dropRight(1)).map(GraveAccentAtom) | s"[^(){} ${Regex.quote(ex)}\\s]+".r.map(PlainValue)) named "Values"

  private def unEscapeString(stringParser: Parser[String]): Parser[String] =
    stringParser.flatMap(x =>
      scala.util
        .Try(StringContext.processEscapes(x))
        .fold(ex => err(ex.getLocalizedMessage), success))

  def rawSingleString: Parser[String] = "\"(((\\\\\")|[^\"])*)\"".r.map(_.drop(1).dropRight(1))

  def singleString = unEscapeString(rawSingleString) named "String Literals"

  def tripleQuotedRawString = "\"\"\"([\\S\\s]+)\"\"\"".r.map(_.drop(3).dropRight(3))

  def tripleQuotedString = unEscapeString(tripleQuotedRawString)

  def string = tripleQuotedString | singleString
  def rawString = tripleQuotedRawString | rawSingleString
  private def noPrefixWhiteSpace[T](p: Parser[T]): Parser[T] = in => {
    val source = in.source
    val offset = in.offset
    if (!in.atEnd && source.charAt(offset).isWhitespace) failure("No whitespace expected")(in)
    else p(in)
  }
  private def restorePrefixWhiteSpace(p: Parser[String]): Parser[String] = in => {
    val whiteSpace = """\s+""".r
    val source = in.source
    whiteSpace.findPrefixMatchOf(source.subSequence(in.offset, source.length())) match {
      case Some(matched) => p(in.drop(matched.end)).map(source.subSequence(in.offset, in.offset + matched.end).+)
      case None => p(in)
    }
  }
  def templateString = (sValueExclude("\"") ~ noPrefixWhiteSpace(string >> parseTemplateBody)) map {
    case Value(template) ~ ((parts, args)) => StringTemplate(template, parts, args)
  }
  def templateBody = {
    val plainString: Parser[String] = restorePrefixWhiteSpace("(?:\\$\\$|[^$])*".r.map(_.replace("$$", "$")))
    val expressionString = ("${" ~> sExpression <~ "}") | ("$" ~> "[a-zA-Z0-9_]+".r).map(Value(_))
    plainString ~ rep(expressionString ~ plainString) map {
      case head ~ tails =>
        ((head :: tails.map(_._2)), tails.map(_._1))
    }
  }
  private def parseTemplateBody(body: String) = parseAll(templateBody, body) match {
    case Success(result, _) => success(result)
    case f => err(f.toString)
  }

  def stringLiteral = string | ("raw" ~> rawString)
  def stringValue = stringLiteral.map(StringLiteral) named "String Values"

  def sQuote = "'" ~> sExpression map SQuote

  def sUnquote = "~" ~> sExpression map SUnQuote

  def sAtom = ":" ~> noPrefixWhiteSpace(success(Nil)) ~> (
    string.map(SAtomLeaf) | sValue.map { case Value(s) => SAtomLeaf(s) }
  )

  def sExpression: Parser[SimpleLispTree] = locational {
    ("(" ~> rep(sExpression) <~ ")" map SList) | stringValue | lambdaHelper | sQuote | sUnquote | sAtom | templateString | sValue
  }

  def sExpressionOrNil = sExpression | success(SList(Nil))

  def eof = "\\z".r named "End of line"

  def root[A](p: Parser[A]) = p <~ eof

  def integer = "\\d+".r map (_.toInt)

  def lambdaHelper = "&" ~> ((sValueExclude("/") ~ ("/" ~> integer) ^^ {
    case s ~ i =>
      val argsList = (0 until i).toList map (x => s"arg$x") map (PlainSymbol)
      PrecompiledSExpression(LambdaExpression(Apply(Evaluator.compile(s), argsList), argsList))
  }) | sExpression.map(ex => {
    val variables = ex.collectVariables.filter(_.matches("#\\d*")).toIndexedSeq.sortBy {
      case "#" => -1
      case s"#$i" => i.toInt
    }.map(PlainSymbol).toList
    PrecompiledSExpression(LambdaExpression(Evaluator.compile(ex),
      if(variables.isEmpty) LisaList(Symbol("...") :: Symbol("_") :: Nil)::Nil else variables))
  }))
}
