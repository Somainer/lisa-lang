package moe.roselia.lisa
import scala.language.dynamics

object RecordType {
  trait Record[K, +V] extends Dynamic {
    def selectDynamic(key: K): V
    def getOrElse[EV >: V](key: K, otherwise: => EV): EV
    def recordTypeName: String
    def indented(bySpace: Int, level: Int): String
  }

  trait MapRecord[K, +V] extends Record[K, V] {
    def record: Map[K, V]

    override def getOrElse[EV >: V](key: K, otherwise: => EV): EV =
      record.getOrElse(key, otherwise)

    override def selectDynamic(key: K): V =
      getOrElse(key,
        throw new NoSuchFieldException(s"$key does not exist on ${if(recordTypeName.isEmpty) "record" else recordTypeName}."))

    override def indented(bySpace: Int, level: Int): String = {
      def withIndent(string: String, level: Int = level) = " ".repeat(bySpace * level) + string
      def withIndentByLines(string: String, level: Int = level) =
        string.split("\n").map(withIndent(_, 1)).mkString("\n")

      val body = record.map {
        case (key, value) =>
          val keyPart = key.toString
          val valuePart = value match {
            case record: Record[_, _] => record.indented(bySpace, level + 1).dropWhile(_ == ' ')
            case otherwise => otherwise.toString
          }

          s"${withIndent(keyPart, level + 1)}: $valuePart"
      }.mkString(",\n")

      withIndentByLines(s"""
                    |$recordTypeName {
                    |$body
                    |}
                    |""".stripMargin.strip())
    }
  }

}
