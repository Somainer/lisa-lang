package moe.roselia.lisa.Compile.Phases

import moe.roselia.lisa.Environments.{EmptyEnv, Environment}
import moe.roselia.lisa.LispExp.Expression

trait Phase {
  def transform(expression: Expression): Expression = transform(expression, EmptyEnv)
  def transform(expression: Expression, environment: Environment): Expression = transform(expression)
}
