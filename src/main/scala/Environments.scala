object Environments {
  sealed trait Environment {
    def has(key: String): Boolean
    def getValueOption(key: String): Option[LispExp.Expression]
    def newFrame = Env(Map.empty, this)
    def withValue(key: String, value: LispExp.Expression): Env = newFrame withValue (key, value)
    def withValues(context: Seq[(String, LispExp.Expression)]): Env = newFrame withValues context
  }
  case class Env(env: Map[String, LispExp.Expression], parent: Environment) extends Environment {
    override def has(key: String): Boolean = env.contains(key) || parent.has(key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      env.get(key).orElse(parent getValueOption key)

    override def withValue(key: String, value: LispExp.Expression): Env = copy(env + (key -> value))

    override def withValues(context: Seq[(String, LispExp.Expression)]): Env = copy(env ++ context)
  }
  object EmptyEnv extends Environment {
    override def has(key: String): Boolean = false

    override def getValueOption(key: String): Option[LispExp.Expression] = None

    override def toString: String = "{}"

  }

  case class CombineEnv(env: Seq[Environment]) extends Environment {
    override def has(key: String): Boolean = env.exists(_ has key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      env.find(_ has key).flatMap(_ getValueOption key)

  }

  abstract class SpecialEnv extends Environment
}
