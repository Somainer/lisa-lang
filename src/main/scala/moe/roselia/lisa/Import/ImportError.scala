package moe.roselia.lisa.Import

trait ImportError extends Error

case class LisaFileNotFound(fileName: String) extends Error(s"Can not import because $fileName not found") with ImportError
case class VariableNotFound(name: String, fileName: String) extends Error(s"Can not import because $name is not defined in $fileName") with ImportError
