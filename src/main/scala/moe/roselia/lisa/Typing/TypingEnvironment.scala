package moe.roselia.lisa.Typing

import moe.roselia.lisa.Environments.SpecialEnv
import moe.roselia.lisa
import moe.roselia.lisa.Exceptions.LisaRuntimeException
import moe.roselia.lisa.LispExp
import moe.roselia.lisa.LispExp.{Expression, NilObj, PrimitiveFunction, SBool, Symbol}

class TypingEnvironment extends SpecialEnv {
  private val cache = collection.mutable.Map.empty[String, Expression]
  private var packageToSearch = lisa.Reflect.ConstructorCaller.searchInPackages.toList
  cache.addAll(Seq(
    "Any" -> LisaType.any,
    "Nothing" -> LisaType.nothing,
    "Type" -> LisaType.itself,
    "Nil" -> LisaType.nil,
    "Tuple" -> TupleTypeConstructor,
    "List" -> SequenceTypeConstructor,
    "Literal" -> PrimitiveFunction.withArityChecked(1) {
      case x :: Nil =>
        if (x.lisaType.isInstanceOf[Untyped]) throw new IllegalArgumentException(s"type of that argument is not inferred")
        x.lisaType match {
          // Types with only one possible value should return itself.
          case NullType => NullType
          case NothingType => NothingType
          case LisaType.nil => LisaType.nil
          case _ =>
            LiteralType(x, x.lisaType)
        }
    },
    "Fn" -> FnTypeConstructor,
    "using" -> PrimitiveFunction.withArityChecked(1) {
      case Symbol(x) :: Nil =>
        packageToSearch = x :: packageToSearch
        NilObj
      case _ => throw new IllegalArgumentException("using: expected a symbol")
    }.asMacro,
    "is-instance?" -> PrimitiveFunction.withArityChecked(2) {
      case obj :: (lt: LisaType) :: Nil =>
        SBool(obj.lisaType <:< lt)
      case _ :: lt :: Nil =>
        throw LisaRuntimeException(lt, new IllegalArgumentException(s"expected a type but ${lt.lisaType.name} found."))
    }
  ))

  private def resolveType(name: String): Option[Expression] = {
    try {
      val clazz = lisa.Reflect.ConstructorCaller.resolveClassBySimpleName(name, packageToSearch)
      Some(LisaType.of(clazz))
    } catch {
      case _: ClassNotFoundException => None
    }
  }

  override def getValueOption(key: String): Option[LispExp.Expression] = {
    if (cache.contains(key)) Some(cache(key))
    else resolveType(key) match {
      case Some(ex) => cache.update(key, ex); Some(ex)
      case None => None
    }
  }

  override def collectDefinedValues: Set[String] =
    cache.keySet.toSet // Getting values of cached values will not cause reflection call.
}
object TypingEnvironment extends TypingEnvironment
