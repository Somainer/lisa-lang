package moe.lisa.util

import scala.language.implicitConversions
import scala.quoted._
import scala.compiletime.error

case class RGBColor(red: Short, green: Short, blue: Short)

object RGBColor {
  def fromHexString(hex: String): Option[RGBColor] = {
    Some(hex.stripPrefix("#")) collect {
      case s if s.length == 3 => s.flatMap(c => s"$c$c")
      case s if s.length == 6 => s
    } map { x =>
      val it = x.toSeq.sliding(2, 2).map(_.unwrap).map(java.lang.Short.parseShort(_, 16))
      fromSeq(it.toSeq)
    }
  }

  def parseHexString(hex: String): RGBColor = fromHexString(hex).get

  def fromSeq(seq: Seq[Short]): RGBColor = {
    val Seq(r, g, b) = seq.ensuring(_.length == 3, s"Sequence must have just 3 elements, but ${seq.length} got.")
    RGBColor(r, g, b)
  }

  given ToExpr[RGBColor] with
    override def apply(x: RGBColor)(using Quotes): Expr[RGBColor] =
      val RGBColor(r, g, b) = x
      '{ RGBColor(${Expr(r)}, ${Expr(g)}, ${Expr(b)}) }

  inline def ofLiteral(inline hex: String): RGBColor = ${ ofLiteralImpl('hex) }

  private[util] def ofLiteralImpl(hex: Expr[String])(using Quotes): Expr[RGBColor] =
    import quotes.reflect.report
    val hexStr = hex.valueOrError
    val rgb = scala.util.Try(fromHexString(hexStr)).toOption.flatten match
      case Some(rgb) => rgb
      case _ => report.throwError(s"$hexStr is not a hex string.")
    Expr(rgb)
}

trait ConsoleColor {


  /**
   * [[ANSIConstants]] contains SGR parameter constants.
   * @see https://stackoverflow.com/questions/4842424/list-of-ansi-color-escape-sequences
   */
  object ANSIConstants {
    final val reset = 0
    final val bold = 1
    final val faint = 2
    final val italic = 3
    final val underline = 4
    final val setForegroundTrueColor = 38
    final val defaultForegroundColor = 39
    final val setBackgroundTrueColor = 48
    final val defaultBackgroundColor = 49

    final val ansiBlack = 30
    final val ansiRed = 31
    final val ansiGreen = 32
    final val ansiYellow = 33
    final val ansiBlue = 34
    final val ansiMagenta = 35
    final val ansiCyan = 36
    final val ansiWhite = 37

    final val reversed = 7
    final val invisible = 8

    /**
     * Return the reverse flag of SGR parameters.
     * Some reverse parameters are not widely supported, like the reverse flag of bold (1),
     * using 22 instead.
     * @param flag The SGR flag.
     * @return [[Some]] if such parameter exists otherwise [[None]].
     * @see https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
     */
    def reverseFlagOf(flag: Int): Option[Int] = flag match {
      case 1 | 2 => Some(22)
      case f if f >= 1 && f <= 9 => Some(f + 20)
      case `setForegroundTrueColor` => Some(defaultForegroundColor)
      case `setBackgroundTrueColor` => Some(defaultBackgroundColor)
      case _ => None
    }
  }

  /**
   * The console color wrapper.
   * @param payload The text we want to display.
   * @param foreground The foreground color.
   * @param background The background color.
   * @param additionalArguments The control parameters, they should be values in [[ANSIConstants]].
   */
  case class ConsoleColor(
                           payload: String,
                           foreground: Option[RGBColor],
                           background: Option[RGBColor],
                           additionalArguments: List[Int]
                         ) {
    private def withArgument(arg: Int) = copy(additionalArguments = arg :: additionalArguments)
    private def withArguments(arg: List[Int]) = copy(additionalArguments = additionalArguments ::: arg)
    private def resetArguments: List[Int] = {
      val reverseFlags = additionalArguments.flatMap(ANSIConstants.reverseFlagOf)
      // There must be some cases that we could not reset, so we just reset all via the 0 parameter.
      if (reverseFlags.length != additionalArguments.length) List(0)
      else {
        reverseFlags ++
          foreground.map(_ => ANSIConstants.defaultForegroundColor) ++
          background.map(_ => ANSIConstants.defaultBackgroundColor)
      }
    }
    private def toArgumentString = {
      val arguments = collection.mutable.ArrayBuffer.empty[Int]
      foreground.map(ConsoleColor.argumentsOfForeground).foreach(arguments.addAll)
      background.map(ConsoleColor.argumentsOfBackground).foreach(arguments.addAll)
      arguments.addAll(additionalArguments)
      val prefix = ConsoleColor.ansiEscapeString(arguments.toSeq)
      val reset = ConsoleColor.ansiEscapeString(resetArguments)
      s"$prefix$payload$reset"
    }

    def bold: ConsoleColor = withArgument(ANSIConstants.bold)
    def faint: ConsoleColor = withArgument(ANSIConstants.faint)
    def underline: ConsoleColor = withArgument(ANSIConstants.underline)
    def italic: ConsoleColor = withArgument(ANSIConstants.italic)
    def background(color: RGBColor): ConsoleColor = copy(background = Some(color))
    def foreground(color: RGBColor): ConsoleColor = copy(foreground = Some(color))
    def reversed: ConsoleColor = withArgument(ANSIConstants.reversed)
    def invisible: ConsoleColor = withArgument(ANSIConstants.invisible)

    def ansiBlack: ConsoleColor = withArgument(ANSIConstants.ansiBlack)
    def ansiRed: ConsoleColor = withArgument(ANSIConstants.ansiRed)
    def ansiGreen: ConsoleColor = withArgument(ANSIConstants.ansiGreen)
    def ansiYellow: ConsoleColor = withArgument(ANSIConstants.ansiYellow)
    def ansiBlue: ConsoleColor = withArgument(ANSIConstants.ansiBlue)
    def ansiMagenta: ConsoleColor = withArgument(ANSIConstants.ansiMagenta)
    def ansiCyan: ConsoleColor = withArgument(ANSIConstants.ansiCyan)
    def ansiWhite: ConsoleColor = withArgument(ANSIConstants.ansiWhite)

    override def toString: String = toArgumentString
  }
  object ConsoleColor {
    def ansiEscapeString(ofArguments: Seq[String]) = s"\u001b[${ofArguments.mkString(";")}m"
    def ansiEscapeString[T](ofArguments: Seq[T])(using T => Int): String =
      ansiEscapeString(ofArguments.map(_.toString))
    val reset: String = ansiEscapeString(ANSIConstants.reset :: Nil)
    def argumentsOfColor(color: RGBColor): List[Int] = {
      List(2, color.red, color.green, color.blue)
    }
    def argumentsOfBackground(color: RGBColor): List[Int] = ANSIConstants.setBackgroundTrueColor :: argumentsOfColor(color)
    def argumentsOfForeground(color: RGBColor): List[Int] = ANSIConstants.setForegroundTrueColor :: argumentsOfColor(color)
  }

  object Implicits {
    implicit inline def hexStringToRGBColor(inline hex: String): RGBColor = RGBColor.ofLiteral(hex)
    implicit def wrapTextToConsoleColor(text: String): ConsoleColor = ConsoleColor(text, None, None, Nil)
    implicit def consoleColorToString(consoleColor: ConsoleColor): String = consoleColor.toString
  }
}

/**
 * The Singleton of [[ConsoleColor]], for most cases, just import [[ConsoleColor.Implicits]], then
 * the implicit conventions will convert [[String]] to [[ConsoleColor.RGBColor]] and convert [[String]] to [[ConsoleColor]] instants when needed.
 */
object ConsoleColor extends ConsoleColor