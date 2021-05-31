package moe.roselia.lisa.Compile.Phases
import moe.roselia.lisa.{Environments, LispExp}

object Pipeline {
  def chain(first: Phase, second: Phase): Phase = new Phase {
    override def transform(expression: LispExp.Expression, environment: Environments.Environment): LispExp.Expression =
      second.transform(first.transform(expression, environment), environment)
  }

  implicit class PhaseExtension(val phase: Phase) extends AnyVal {
    def andThen(other: Phase): Phase =
      chain(phase, other)
  }

  object EmptyPhase extends Phase {
    override def transform(expression: LispExp.Expression): LispExp.Expression = expression
  }
}
