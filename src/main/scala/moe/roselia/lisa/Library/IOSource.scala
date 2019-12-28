package moe.roselia.lisa.Library
import scala.io.Source
import java.net.{URI, URL}

import moe.roselia.lisa.Environments.{Environment, SpecialEnv}
import moe.roselia.lisa.Reflect.PackageAccessor.ObjectEnv

object IOSource {
  object SourceLibraryImpl {
    def readFileContent(fileName: String): String = {
      Source.fromFile(fileName).mkString
    }
    def readFromUri(uri: String): String = {
      Source.fromURI(new URI(uri)).mkString
    }
    def readFromUrl(url: String): String = {
      Source.fromURL(new URL(url)).mkString
    }
    def toJson(json: String) = {
      util.parsing.json.JSON.parseFull(json)
    }
  }

  lazy val sourceLibrary: Environment =
    SpecialEnv.cached(ObjectEnv.ofKebabCase(SourceLibraryImpl))
}
