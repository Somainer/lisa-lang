package moe.lisa.util

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants._
import scala.annotation.switch
import java.lang.Integer.toOctalString

object StringLiteral extends DottyContext {
  def escapedChar(ch: Char): String = (ch: @switch) match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ => if (ch.isControl) "\\0" + toOctalString(ch) else String.valueOf(ch)
  }

  def escapedString(str: String): String = str flatMap escapedChar

  def toLiteralCode(s: String): String =
    s"\"${escapedString(s)}\""
}
