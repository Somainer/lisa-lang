package moe.lisa.util

object PathUtil {
  def getExtension(name: String): String =
    var i = name.length - 1
    while i >= 0 && name.charAt(i) != '.' do
      i -= 1
    
    if i < 0 then ""
    else name.substring(i + 1).toLowerCase()
    
  def getClassPath = System.getProperty("java.class.path")
}
