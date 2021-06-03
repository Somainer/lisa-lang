package moe.lisa.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import dotty.tools.dotc.ast.Positioned
import dotty.tools.dotc.util.Spans.Span
import moe.lisa.core.SourceFile._
import moe.lisa.core.context.Context
import moe.lisa.core.expression.Constant
import moe.lisa.core.expression.Tree
import moe.lisa.core.expression.UntypedTree._
import moe.lisa.core.expression.Properties._
import moe.lisa.core.expression.UntypedTree
import moe.lisa.lang.{Atom, Symbol => LSymbol}

trait LisaParser(using SourceFile) extends RegexParsers {
  override protected val whiteSpace = """(\s|;.*)+""".r

  object numbers extends NumberParser

  implicit def foreignParser[T](p: numbers.Parser[T]): Parser[T] = Parser { in =>
    p(in.asInstanceOf[numbers.Input]) match
      case numbers.Success(r, n) => Success(r, n)
      case numbers.Error(e, n) => Error(e, n)
      case numbers.Failure(e, n) => Failure(e, n)
  }

  def locational[T <: Positioned](p: Parser[T], handleSpaces: Boolean = true): Parser[T] = Parser { in =>
    val offset = in.offset
    val start = if handleSpaces then handleWhiteSpace(in.source, offset) else offset
    p(in.drop(start - offset)) match
      case Success(r, next) =>
        val end = next.offset
        val span = Span(start, end)
        val positioned = r.withSpan(span)
        Success(positioned, next)
      case f => f
  }

  private def toLiteral(value: Any): Literal = Literal(Constant(value))

  def sSymbol: Parser[Symbol] = sSymbolExclude(":;&,")
  def sSymbolExclude(ex: String): Parser[Symbol] =
    ("`.+`".r.map(_.drop(1).dropRight(1)).map(Symbol).map(_.withAttachment(BackQuoted, ())) | s"[^(){}\\[\\] ${Regex.quote(ex)}\\s]+".r.map(Symbol)) named "Values"

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
//    val whiteSpace = """\s+""".r
    val source = in.source
    whiteSpace.findPrefixMatchOf(source.subSequence(in.offset, source.length())) match {
      case Some(matched) => p(in.drop(matched.end)).map(source.subSequence(in.offset, in.offset + matched.end).toString.+)
      case None => p(in)
    }
  }

  def templateString = (locational(sSymbolExclude("\"")) ~ (noPrefixWhiteSpace(parseTemplateBody(string)))) map {
    case (template: Symbol) ~ ((parts, args)) => StringTemplate(template, parts, args)
  }
  def templateBody = {
    val plainString: Parser[Literal] =
      locational(restorePrefixWhiteSpace("(?:\\$\\$|[^$])*".r.map(_.replace("$$", "$"))) ^^ toLiteral, handleSpaces = false)
    val expressionString = ("${" ~> sExpression <~ "}") | locational(("$" ~> "[a-zA-Z0-9_]+".r).map(Symbol(_)))
    plainString ~ rep(expressionString ~ plainString) map {
      case head ~ tails =>
        ((head :: tails.map(_._2)), tails.map(_._1))
    }
  }
  private def parseTemplateBody(input: Parser[String]) = Parser { in =>
    def advance[T <: Positioned, U <: Positioned](byLoc: T, offset: Int = 0)(loc: T): T = {
      val bySpan = byLoc.span
      val span = loc.span
      val start = bySpan.start + span.start + offset
      val end = bySpan.end + span.end + offset
      loc.withSpan(Span(start, end))
    }
    val literal = locational(input.map(toLiteral))
    literal(in) match {
      case Success(body, next) =>
        val parsed = body.const.stringValue
        val offset = parsed.indexWhere(_ != '"')
        parseAll(templateBody, parsed) match {
          case Success((literals, bodies), _) =>
            Success((literals.map(advance(body, offset)), bodies.map(advance(body, offset))), next)
          case f => f
        }
      case ns => failure(ns.toString)(in)
    }
  }

  def stringLiteral = string | ("raw" ~> rawString)
  def stringValue: Parser[Literal] = stringLiteral.map(toLiteral) named "String Values"

  def sQuote: Parser[Quote] = {
    ("'" ~> sExpression map (Quote(_))) |
      ("`'" ~> sExpression map (Quote(_)) map (_.addMarker(QuasiSplicing)))
  }

  def sUnquote: Parser[UnQuote] =
    ("~..." ~> sExpression map (UnQuote(_)) map (_.addMarker(QuasiSplicing))) |
      ("~" ~> sExpression map (UnQuote(_)))

  def sAtom: Parser[Atom] = ":" ~> noPrefixWhiteSpace(success(Nil)) ~> (
    string.map(Atom(_)) | sSymbol.map { case Tree.Symbol(LSymbol(s)) => Atom(s) }
  )

  def sAtomTree: Parser[Literal] = sAtom map toLiteral

  def sRoundList: Parser[LisaList] =
    ("(" ~> rep(sExpression) <~ ")").map(LisaList).map(_.withAttachment(BracketType, BracketKind.Round))

  def sSquareList: Parser[LisaList] =
    ("[" ~> rep(sExpression) <~ "]").map(LisaList).map(_.withAttachment(BracketType, BracketKind.Square))

  def sRecordLiteral: Parser[RecordLiteral] =
    ("{" ~> rep(sExpression) <~ "}").map(RecordLiteral(_))

  def simpleLiterals: Parser[Tree] =
    dotLiterals | numbers.numberLiteral | booleanTree | stringValue | sAtomTree

  def dotLiterals: Parser[Symbol] =
    consumed("." ~> not(numbers.digit) ~> sSymbol).map(Symbol(_))

  def literals: Parser[Tree] =
    simpleLiterals
    | sRoundList
    | sSquareList
    | sRecordLiteral
    | sQuote
    | sUnquote
    | templateString
    | lambdaLiteral

  def typedTree: Parser[Tree] =
    "(" ~> (sExpression <~ ":") ~ sExpression <~ ")" map {
      case expr ~ typ => UntypedTree.LisaList(List(expr, toLiteral(Atom("typed")), typ))
    }

  def sExpression: Parser[Tree] = locational {
//    ("(" ~> rep(sExpression) <~ ")" map LisaList) | stringValue | lambdaHelper | sQuote | sUnquote | sAtom | templateString | sValue
    literals | typedTree | sSymbol
  }

  def boolean: Parser[Boolean] = ("true" ^^^ true) | ("false" ^^^ false)
  def booleanTree: Parser[Literal] = boolean map toLiteral

  def sExpressionOrNil = sExpression | success(LisaList(Nil))

  def eof = "\\z".r named "End of line"

  def root[A](p: Parser[A]) = p <~ eof

  def consumed(p: Parser[Any]): Parser[String] = Parser { in =>
    p(in) match {
      case Success(_, next) =>
        Success(in.source.subSequence(in.offset, next.offset).toString, next)
      case ns: NoSuccess => ns
    }
  }

  def integer = "\\d+".r map (_.toInt)

  def lambdaLiteral = "&" ~> ((sSymbolExclude("/") ~ ("/" ~> integer) ^^ {
    case s ~ i =>
      val argsList = (0 until i).toList map (x => s"arg$x") map (UntypedTree.Symbol(_))
      UntypedTree.LambdaExpression(UntypedTree.CaseDef(
        UntypedTree.LisaList(argsList),
        UntypedTree.EmptyTree,
        UntypedTree.LisaList(s :: argsList)
      ))
  }) | sExpression.map(ex => {
    UntypedTree.LambdaLiteral(ex)
  }))
}

object LisaParser:
  def summon(using ctx: Context): LisaParser =
    new StringLisaParser(using ctx.source)

  def ofSource(using SourceFile): LisaParser = new StringLisaParser

  def dummy: LisaParser = new StringLisaParser(using NoSourceFile)

class StringLisaParser(using SourceFile) extends LisaParser
