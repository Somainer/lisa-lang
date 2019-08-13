package moe.roselia.lisa

object Environments {
  private val MutableMap = collection.mutable.Map
  private type MutableMap[K, V] = collection.mutable.Map[K, V]
  sealed trait Environment {
    def has(key: String): Boolean = getValueOption(key).isDefined
    def getValueOption(key: String): Option[LispExp.Expression]
    def newFrame = Env(Map.empty, this)
    def withValue(key: String, value: LispExp.Expression): Env = newFrame withValue (key, value)
    def withValues(context: Seq[(String, LispExp.Expression)]): Env = newFrame withValues context
    def newMutableFrame = MutableEnv(MutableMap.empty, this)
    def directHas(key: String) = false
    def forceUpdated(key: String, value: LispExp.Expression): Environment = this
  }
  case class Env(env: Map[String, LispExp.Expression], parent: Environment) extends Environment {
    override def has(key: String): Boolean = directHas(key) || parent.has(key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      env.get(key).orElse(parent getValueOption key)

    override def withValue(key: String, value: LispExp.Expression): Env = copy(env + (key -> value))

    override def withValues(context: Seq[(String, LispExp.Expression)]): Env = copy(env ++ context)

    override def directHas(key: String): Boolean = env.contains(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      if (directHas(key)) copy(env=env.updated(key, value))
      else parent.forceUpdated(key, value)
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

    override def forceUpdated(key: String, value: LispExp.Expression): Environment = {
      def updateChain(e: Seq[Environment]): Seq[Environment] = e match {
        case x::xs =>
          if(x.directHas(key)) x.forceUpdated(key, value)::xs
          else x +: updateChain(xs)
        case _ => Nil
      }
      copy(updateChain(env))
    }
  }

  object CombineEnv {
    def of(env: Environment*) = CombineEnv(env.toSeq)
  }

  abstract class SpecialEnv extends Environment

  case class MutableEnv(private val env: MutableMap[String, LispExp.Expression],
                        parent: Environment) extends Environment {
    override def has(key: String): Boolean = directHas(key) || parent.has(key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      env.get(key).orElse(parent getValueOption key)

    def addValue(key: String, value: LispExp.Expression): MutableEnv = {
      env.update(key, value)
      this
    }

    override def directHas(key: String): Boolean = env.contains(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      if(directHas(key)) addValue(key, value)
      else parent.forceUpdated(key, value)
  }

  object MutableEnv {
    def createEmpty = MutableEnv(MutableMap.empty, EmptyEnv)
  }

  case class NameSpacedEnv(nameSpace: String, env: Environment, separator: String = ".") extends Environment {
    private val prefix = if (nameSpace.isEmpty) "" else s"$nameSpace$separator"
    private val prefixLength = prefix.length
    override def has(key: String): Boolean = key.startsWith(prefix) && env.has(key.substring(prefixLength))

    override def getValueOption(key: String): Option[LispExp.Expression] =
      if (has(key)) env.getValueOption(key.substring(prefixLength))
      else None

    override def directHas(key: String): Boolean =
      has(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      env.forceUpdated(key, value)
  }

  case class TransparentLayer(layer: Environment, base: Environment) extends Environment {
    override def has(key: String): Boolean = layer.has(key) || base.has(key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      layer.getValueOption(key).orElse(base.getValueOption(key))

    override def directHas(key: String): Boolean = base.directHas(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      if(layer.has(key)) layer.forceUpdated(key, value)
      else base.forceUpdated(key, value)
  }
}
