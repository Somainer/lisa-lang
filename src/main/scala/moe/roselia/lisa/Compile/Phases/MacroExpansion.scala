package moe.roselia.lisa.Compile.Phases
import moe.roselia.lisa.Environments.Environment
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Util.Extractors.RichOption

object MacroExpansion extends Phase with ExpressionTraverse {
  private def isMacro(expression: Expression): Boolean = expression match {
    case _: SimpleMacroClosure => true
    case PolymorphicExpression(_, _, _, true) => true
    case _ => false
  }
  private def expandMacro(expression: Expression, args: Seq[Expression], environment: Environment): Expression = expression match {
    case macroClosure: SimpleMacroClosure =>
      Evaluator.expandMacro(macroClosure, args.map(_.toRawList), environment)
    case pm @ PolymorphicExpression(_, _, _, true) =>
      pm.findMatch(args, environment).collect {
        case (mac: SimpleMacroClosure, _) =>
          expandMacro(mac, args, environment)
      }.getOrThrow(new IllegalArgumentException(s"Can not expand macro $pm: no matching macro to call."))
    case _ => throw new IllegalArgumentException(s"$expression is not a macro.")
  }

  override def transform(expression: Expression, environment: Environment): Expression = {
    traverseExpressionBody(expression) {
      case Apply(Symbol(mac), args) if environment.getValueOption(mac).exists(isMacro) =>
        expandMacro(environment.get(mac), args, environment) :: Nil
      case Apply(mac, args) if isMacro(mac) =>
        expandMacro(mac, args, environment) :: Nil
    }
  }
}
