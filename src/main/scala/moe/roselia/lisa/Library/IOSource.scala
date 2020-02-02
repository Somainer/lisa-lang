package moe.roselia.lisa.Library
import scala.io.Source
import scala.util.Using
import java.net.{HttpURLConnection, URI, URL}

import moe.roselia.lisa.Environments.{Environment, SpecialEnv}
import moe.roselia.lisa.LispExp.{Expression, LisaList, LisaRecordWithMap, NilObj}
import moe.roselia.lisa.Reflect.PackageAccessor.ObjectEnv
import moe.roselia.lisa.Reflect.ScalaBridge.{jsonLikeToLisa, toScalaNative}

object IOSource {
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
      util.parsing.json.JSON.parseFull(json).map(jsonLikeToLisa)
    }

    def parseJson(json: String): Expression = {
      parseJsonOption(json).getOrElse(throw new IllegalArgumentException("Input is not a valid json"))
    }

    @moe.roselia.lisa.Annotation.RawLisa
    def toJsonString(ex: Expression): String = {
      import util.parsing.json._
      def transform(exp: Expression): Any = exp match {
        case LisaList(ll) => JSONArray(ll.map(transform))
        case rm: LisaRecordWithMap[_] => JSONObject(rm.record.transform((_, v) => transform(v)))
        case nil if nil eq NilObj => null
        case e => toScalaNative(e)
      }
      transform(ex).toString
    }
  }

  lazy val sourceLibrary: Environment =
    SpecialEnv.cached(ObjectEnv.ofKebabCase(SourceLibraryImpl))
}
