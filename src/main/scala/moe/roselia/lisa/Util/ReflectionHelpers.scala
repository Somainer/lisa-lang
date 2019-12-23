package moe.roselia.lisa.Util

import scala.util.Try

object ReflectionHelpers {
  def collectException[Ex <: Throwable](ex: Ex): String = {
    val cause = ex.getCause
    val sb = new StringBuilder
    sb.append(ex.toString)
    if (cause ne null) sb.append(s"(Caused by $cause)")
    sb.toString()
  }

  def tryApplyOnObjectReflective(obj: Any, arguments: Seq[Any]): Try[Any] = {
    Try {
      obj.asInstanceOf[Function[Seq[Any], Any]](arguments)
    }.orElse(
      Try {
        obj.asInstanceOf[{def apply(any: Any*): Any}].apply(arguments: _*)
      }
    ).orElse{
      Try {
        arguments match {
          case x::Nil => obj.asInstanceOf[{def apply(any: Any): Any}].apply(x)
          case x::y::Nil => obj.asInstanceOf[{def apply(a1: Any, a2: Any): Any}].apply(x, y)
          case x::y::z::Nil => obj.asInstanceOf[{def apply(a1: Any, a2: Any, a3: Any)}].apply(x, y, z)
        }
      }
    }
  }

  def foldExceptionToEither[T](t: Try[T]): Either[String, T] =
    t.fold(ex => Left(collectException(ex)), Right(_))

  def tryToEither[T](t: => T): Either[String, T] = foldExceptionToEither(Try(t))
}
