package moe.roselia.lisa.Library
import scala.io.Source
import scala.util.Using
import java.net.{HttpURLConnection, URI, URL}

import moe.roselia.lisa.Environments.{Environment, SpecialEnv}
import moe.roselia.lisa.LispExp.{Expression, LisaList, LisaMapRecord, LisaRecordWithMap, NilObj}
import moe.roselia.lisa.Reflect.PackageAccessor.ObjectEnv
import moe.roselia.lisa.Reflect.ScalaBridge.{jsonLikeToLisa, toScalaNative}

object IOSource {

  /** Simple JSON parser to replace scala.util.parsing.json.JSON which is not available in Scala 3 */
  private object SimpleJsonParser {
    def parseFull(json: String): Option[Any] = {
      try {
        val (result, rest) = parseValue(json.trim, 0)
        if (rest >= json.trim.length) Some(result) else None
      } catch {
        case _: Exception => None
      }
    }

    private def parseValue(s: String, pos: Int): (Any, Int) = {
      val p = skipWhitespace(s, pos)
      if (p >= s.length) throw new IllegalArgumentException("Unexpected end of input")
      s.charAt(p) match {
        case '"' => parseString(s, p)
        case '{' => parseObject(s, p)
        case '[' => parseArray(s, p)
        case 't' => ("true".ensuring(s.startsWith("true", p)), true, p + 4) match { case (_, v, i) => (v, i) }
        case 'f' => ("false".ensuring(s.startsWith("false", p)), false, p + 5) match { case (_, v, i) => (v, i) }
        case 'n' => ("null".ensuring(s.startsWith("null", p)), null, p + 4) match { case (_, v, i) => (v, i) }
        case c if c == '-' || c.isDigit => parseNumber(s, p)
        case c => throw new IllegalArgumentException(s"Unexpected character '$c' at position $p")
      }
    }

    private def skipWhitespace(s: String, pos: Int): Int = {
      var p = pos
      while (p < s.length && s.charAt(p).isWhitespace) p += 1
      p
    }

    private def parseString(s: String, pos: Int): (String, Int) = {
      val sb = new StringBuilder
      var p = pos + 1 // skip opening quote
      while (p < s.length && s.charAt(p) != '"') {
        if (s.charAt(p) == '\\') {
          p += 1
          s.charAt(p) match {
            case '"' => sb.append('"')
            case '\\' => sb.append('\\')
            case '/' => sb.append('/')
            case 'b' => sb.append('\b')
            case 'f' => sb.append('\f')
            case 'n' => sb.append('\n')
            case 'r' => sb.append('\r')
            case 't' => sb.append('\t')
            case 'u' =>
              val hex = s.substring(p + 1, p + 5)
              sb.append(Integer.parseInt(hex, 16).toChar)
              p += 4
            case c => throw new IllegalArgumentException(s"Invalid escape: \\$c")
          }
        } else {
          sb.append(s.charAt(p))
        }
        p += 1
      }
      (sb.toString, p + 1) // skip closing quote
    }

    private def parseNumber(s: String, pos: Int): (Any, Int) = {
      var p = pos
      if (p < s.length && s.charAt(p) == '-') p += 1
      while (p < s.length && s.charAt(p).isDigit) p += 1
      var isFloat = false
      if (p < s.length && s.charAt(p) == '.') { isFloat = true; p += 1; while (p < s.length && s.charAt(p).isDigit) p += 1 }
      if (p < s.length && (s.charAt(p) == 'e' || s.charAt(p) == 'E')) {
        isFloat = true; p += 1
        if (p < s.length && (s.charAt(p) == '+' || s.charAt(p) == '-')) p += 1
        while (p < s.length && s.charAt(p).isDigit) p += 1
      }
      val numStr = s.substring(pos, p)
      val num: Any = numStr.toDouble // always use Double like the old scala.util.parsing.json
      (num, p)
    }

    private def parseObject(s: String, pos: Int): (Map[String, Any], Int) = {
      var p = skipWhitespace(s, pos + 1)
      val map = scala.collection.mutable.LinkedHashMap.empty[String, Any]
      if (p < s.length && s.charAt(p) == '}') return (map.toMap, p + 1)
      while (true) {
        p = skipWhitespace(s, p)
        val (key, p2) = parseString(s, p)
        p = skipWhitespace(s, p2)
        require(s.charAt(p) == ':', s"Expected ':' at position $p")
        p = skipWhitespace(s, p + 1)
        val (value, p3) = parseValue(s, p)
        map(key) = value
        p = skipWhitespace(s, p3)
        if (s.charAt(p) == '}') return (map.toMap, p + 1)
        require(s.charAt(p) == ',', s"Expected ',' or '}' at position $p")
        p += 1
      }
      throw new IllegalStateException("unreachable")
    }

    private def parseArray(s: String, pos: Int): (List[Any], Int) = {
      var p = skipWhitespace(s, pos + 1)
      val list = scala.collection.mutable.ListBuffer.empty[Any]
      if (p < s.length && s.charAt(p) == ']') return (list.toList, p + 1)
      while (true) {
        p = skipWhitespace(s, p)
        val (value, p2) = parseValue(s, p)
        list += value
        p = skipWhitespace(s, p2)
        if (s.charAt(p) == ']') return (list.toList, p + 1)
        require(s.charAt(p) == ',', s"Expected ',' or ']' at position $p")
        p += 1
      }
      throw new IllegalStateException("unreachable")
    }
  }

  /** Simple JSON serializer to replace scala.util.parsing.json classes */
  private object SimpleJsonSerializer {
    def toJsonString(value: Any): String = value match {
      case null => "null"
      case b: Boolean => b.toString
      case n: Number => n.toString
      case s: String =>
        "\"" + s.flatMap {
          case '"' => "\\\""
          case '\\' => "\\\\"
          case '\n' => "\\n"
          case '\t' => "\\t"
          case '\r' => "\\r"
          case '\b' => "\\b"
          case '\f' => "\\f"
          case c if c < ' ' => f"\\u${c.toInt}%04x"
          case c => c.toString
        } + "\""
      case m: Map[_, _] =>
        m.map { case (k, v) => toJsonString(k.toString) + ":" + toJsonString(v) }.mkString("{", ",", "}")
      case l: Iterable[_] =>
        l.map(toJsonString).mkString("[", ",", "]")
      case other => other.toString
    }
  }

  object SourceLibraryImpl {
    def readFileContent(fileName: String): String = {
      Using.resource(Source.fromFile(fileName))(_.mkString)
    }
    def readFromUri(uri: String): String = {
      Using.resource(Source.fromURI(new URI(uri)))(_.mkString)
    }
    def readFromUrl(url: String): String = {
      Using.resource(Source.fromURL(new URL(url)))(_.mkString)
    }

    def parseJsonOption(json: String): Option[Expression] = {
      SimpleJsonParser.parseFull(json).map(jsonLikeToLisa)
    }

    def parseJson(json: String): Expression = {
      parseJsonOption(json).getOrElse(throw new IllegalArgumentException("Input is not a valid json"))
    }

    @moe.roselia.lisa.Annotation.RawLisa
    def toJsonString(ex: Expression): String = {
      def transform(exp: Expression): Any = exp match {
        case LisaList(ll) => ll.map(transform)
        case rm: LisaRecordWithMap[_] => rm.record.map { case (k, v) => (k, transform(v)) }
        case nil if nil eq NilObj => null
        case e => toScalaNative(e)
      }
      SimpleJsonSerializer.toJsonString(transform(ex))
    }
  }

  lazy val sourceLibrary: Environment =
    SpecialEnv.cached(ObjectEnv.ofKebabCase(SourceLibraryImpl))
}
