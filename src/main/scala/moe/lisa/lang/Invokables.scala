package moe.lisa.lang

import moe.lisa.core.expression.UntypedTree
import moe.lisa.util.ThrowHelper

trait IInvokable:
  def invoke(xs: Seq[Any]): Any

trait IDynamicInvokable extends IInvokable:
  private inline def throwArity(n: Int) =
    ThrowHelper.throwArity(n, getClass.getName)

  def apply(): Any = throwArity(0)
  def apply(arg: Any): Any = throwArity(1)
  def apply(arg1: Any, arg2: Any): Any = throwArity(2)
  def apply(arg1: Any, arg2: Any, arg3: Any): Any = throwArity(3)
  def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any): Any = throwArity(4)
  def apply(arg1: Any, arg2: Any, arg3: Any, arg4: Any, arg5: Any): Any = throwArity(5)

  override def invoke(xs: Seq[Any]): Any =
    xs.length match
      case 0 => apply()
      case 1 => apply(xs(0))
      case 2 => apply(xs(0), xs(1))
      case 3 => apply(xs(0), xs(1), xs(2))
      case 4 => apply(xs(0), xs(1), xs(2), xs(3))
      case 5 => apply(xs(0), xs(1), xs(2), xs(3), xs(4))

trait IMacro:
  import UntypedTree._
  def expand(arg: Tree): Tree
