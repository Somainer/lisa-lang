package moe.lisa.core.io

import dotty.tools.io

object Files {
  type AbstractFile = io.AbstractFile
  
  export io.AbstractFile._
  val NoFile = io.NoAbstractFile
}
