package moe.lisa.core.context

import moe.lisa.config.LisaSettings
import moe.lisa.core.SourceFile._
import moe.lisa.core.io.Files.AbstractFile
import moe.lisa.util.AutoClearable

import scala.collection.mutable

class ContextBase extends AutoClearable {
  val settings: LisaSettings = new LisaSettings

  val sources: mutable.HashMap[AbstractFile, SourceFile] = needClear(mutable.HashMap.empty)
}
