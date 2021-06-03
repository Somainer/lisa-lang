package moe.lisa.runtime.reflect

import scala.reflect.ensureAccessible

object DynamicSelect:
  // Perform simple reflection.
  def selectDynamic(obj: Any, name: String): Any =
    val cls = obj.getClass
    try
      val field = cls.getField(name)
      ensureAccessible(field)
      field.get(obj)
    catch
      case _: NoSuchFieldException =>
        applyDynamic(obj, name)()
  end selectDynamic

  def applyDynamic(obj: Any, name: String, paramTypes: Class[?]*)(args: Any*): Any =
    val cls = obj.getClass
    val method = cls.getMethod(name, paramTypes*)
    ensureAccessible(method)
    method.invoke(obj, args*)
  end applyDynamic
