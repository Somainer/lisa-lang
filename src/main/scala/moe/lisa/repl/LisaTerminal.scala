package moe.lisa.repl

import moe.lisa.util.{ConsoleColor, RGBColor}
import ConsoleColor.Implicits._
import org.jline.reader
import org.jline.reader.impl.LineReaderImpl
import org.jline.reader.{Completer, EOFError, LineReader, LineReaderBuilder}
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.{Terminal, TerminalBuilder}

class LisaTerminal extends java.io.Closeable {
  import LisaTerminal._

  val terminal: Terminal = TerminalBuilder
    .builder()
    .name("Lisa").build()
  def dumbTerminal = Option(System.getenv("TERM")) == Some("dumb")
  private def primaryColor(text: String) = text.foreground(mainColor)
  val promptText = primaryColor("lisa>")
  val newLinePromptText = primaryColor("....>")

  val history = new DefaultHistory

  def readLine(completer: Completer)(using ReplState): String = {
    import LineReader._
    import LineReader.Option._

    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .appName("Lisa")
      .history(history)
      .parser(new Parser)
      .completer(completer)
      .variable(WORDCHARS, LineReaderImpl.DEFAULT_WORDCHARS.filterNot("[](){}&#".toSet))
      .variable(SECONDARY_PROMPT_PATTERN, "%M")
      .variable(INDENTATION, 2)
      .option(INSERT_TAB, true)
      .option(AUTO_FRESH_LINE, true)
      .option(DISABLE_EVENT_EXPANSION, true)
      .build()

    reader.readLine(promptText)
  }
  private class Parser(using ReplState) extends reader.Parser {
    class ParsedLine(
                      val cursor: Int, val line: String, val word: String, val wordCursor: Int) extends reader.ParsedLine {
      override def wordIndex(): Int = -1
      override def words(): java.util.List[String] = java.util.Collections.emptyList
    }
    def incomplete(openBrackets: Int): Nothing = throw new EOFError(
      -1, -1, "", newLinePromptText, openBrackets, null
    )
    private def openBrackets(line: String) = line.foldLeft(0)((pairs, c) => {
      if (pairs < 0) pairs
      else if (c == '(') pairs + 1
      else if (c == ')') pairs - 1
      else pairs
    })
    override def parse(line: String, cursor: Int, context: reader.Parser.ParseContext): reader.ParsedLine = {
      def parsedLine(word: String, wordCursor: Int) = new ParsedLine(
        cursor, line, word, wordCursor
      )
      def defaultParsedLine = parsedLine("", 0)
      def isAtEnd = !line.substring(cursor).contains(System.lineSeparator())
      val openPairs = openBrackets(line)
      def isComplete = {
        import moe.lisa.parsing.LisaParser
        val parser = LisaParser.dummy
        import parser._
        parseAll(sExpressionOrNil, line) match {
          case _: Success[_] => true
          case _: Failure => false
          case _: Error => true
        }
      }
      context match {
        //        case reader.Parser.ParseContext.ACCEPT_LINE if openPairs <= 0 =>
        case reader.Parser.ParseContext.ACCEPT_LINE if (isAtEnd && isComplete) || openPairs <= 0 =>
          defaultParsedLine
        case reader.Parser.ParseContext.COMPLETE =>
          val parser = new reader.impl.DefaultParser()
          parser.parse(line, cursor, context)
        case _ =>
          incomplete(openPairs)
      }
    }
  }
  override def close(): Unit = terminal.close()
}

object LisaTerminal:
  val keywords = Seq(
    "define", "define-macro", "lambda", "if", "cond", "let", "true", "false"
  )

  val mainColor: RGBColor = "#6670ed"
  val errorColor: RGBColor = "#ff0000"
