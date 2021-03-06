package moe.roselia.lisa.Repl

import java.util.regex.Pattern

import moe.roselia.lisa.Util.ConsoleColor.ConsoleColor
import org.jline.reader
import org.jline.reader.{Completer, EOFError, LineReader, LineReaderBuilder}
import org.jline.reader.impl.history.DefaultHistory
import org.jline.terminal.{Terminal, TerminalBuilder}
import moe.roselia.lisa.Util.ConsoleColor.Implicits._
import org.jline.reader.impl.LineReaderImpl
import org.jline.utils.AttributedString

class LisaTerminal extends java.io.Closeable {
  import LisaTerminal._

  val terminal: Terminal = TerminalBuilder.builder()
    .name("Lisa")
//    .dumb(false)
    .build()

  val history = new DefaultHistory
  private def primaryColor(text: String): String = text.foreground(roseliaColor)
  private val promptText = primaryColor("lisa>")
  private val newLinePrompt = primaryColor("....>")

  def readLine(completer: Completer)(implicit state: State): String = {
    import LineReader._
    import LineReader.Option._
    val reader = LineReaderBuilder.builder()
      .terminal(terminal)
      .appName("Lisa")
      .history(history)
      .completer(completer)
      .parser(new Parser)
      .highlighter(new Highlighter)
      .variable(WORDCHARS, LineReaderImpl.DEFAULT_WORDCHARS.filterNot("()".toSet))
      .variable(SECONDARY_PROMPT_PATTERN, "%M")
      .variable(INDENTATION, 2)
      .option(INSERT_TAB, true)
      .option(AUTO_FRESH_LINE, true)
      .option(DISABLE_EVENT_EXPANSION, true)
      .build()

    reader.readLine(promptText)
  }

  private class Parser(implicit state: State) extends reader.Parser {
    class ParsedLine(
     val cursor: Int, val line: String, val word: String, val wordCursor: Int) extends reader.ParsedLine {
      override def wordIndex(): Int = -1
      override def words(): java.util.List[String] = java.util.Collections.emptyList
    }
    def incomplete(openBrackets: Int): Nothing = throw new EOFError(
      -1, -1, "", newLinePrompt, openBrackets, null
    )
    private def openBrackets(line: String) = moe.roselia.lisa.Main.indentLevel(line)
    override def parse(line: String, cursor: Int, context: reader.Parser.ParseContext): reader.ParsedLine = {
      def parsedLine(word: String, wordCursor: Int) = new ParsedLine(
        cursor, line, word, wordCursor
      )
      def defaultParsedLine = parsedLine("", 0)
      val openPairs = openBrackets(line)
      def isComplete = {
        import moe.roselia.lisa.SExpressionParser._
        parseAll(sExpressionOrNil, line) match {
          case _: Success[_] => true
          case _: Failure => false
          case _: Error => true
        }
      }
      context match {
//        case reader.Parser.ParseContext.ACCEPT_LINE if openPairs <= 0 =>
        case reader.Parser.ParseContext.ACCEPT_LINE if isComplete || openPairs <= 0 =>
          defaultParsedLine
        case reader.Parser.ParseContext.COMPLETE =>
          val parser = new reader.impl.DefaultParser()
          parser.parse(line, cursor, context)
        case _ =>
          incomplete(openPairs)
      }
    }
  }

  private class Highlighter(implicit state: State) extends reader.Highlighter {
    override def highlight(reader: LineReader, buffer: String): AttributedString = {
      import moe.roselia.lisa.SExpressionParser._

      val sExpressions = rep(sExpression | (rep("(") ~> sExpression))
      parse(sExpressions, buffer) match {
        case NoSuccess(_, input) =>
          val errorPart = buffer.substring(input.offset)
          AttributedString.fromAnsi(buffer.substring(0, input.offset) + errorPart.ansiRed)
        case Success(tree, input) =>
          AttributedString fromAnsi ASTHighlighter.highlightTree(tree, buffer)(state.environment)
      }
    }

    override def setErrorPattern(errorPattern: Pattern): Unit = ()

    override def setErrorIndex(errorIndex: Int): Unit = ()
  }

  def close(): Unit = {
    terminal.close()
//    print(ConsoleColor.reset)
  }
}

object LisaTerminal {
  val roseliaColor = "#6670ed"
  val errorColor = "#ff0000"
  val keywords = Seq(
    "define", "define-macro", "lambda", "if", "cond", "let", "true", "false"
  )

  def provideKeywordHint(word: String): Seq[String] = keywords.filter(_.startsWith(word))
}
