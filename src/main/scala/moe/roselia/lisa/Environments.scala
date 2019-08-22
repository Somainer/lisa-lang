package moe.roselia.lisa

object Environments {
  private val MutableMap = collection.mutable.Map
  private type MutableMap[K, V] = collection.mutable.Map[K, V]
  sealed trait Identifiable {
    var identify: String = ##.toString
    def withIdentify(id: String): this.type = {
      identify = id
      this
    }
    @`inline` def =:=(that: Identifiable): Boolean = identify == that.identify
    @`inline` def =:=(that: String): Boolean = identify == that
    @`inline` def =/=(that: Identifiable): Boolean = ! =:=(that)
    @`inline` def =/=(that: String): Boolean = ! =:=(that)
  }
  sealed trait Environment extends Identifiable {
    def has(key: String): Boolean = getValueOption(key).isDefined
    def getValueOption(key: String): Option[LispExp.Expression]
    def newFrame = Env(Map.empty, this)
    def withValue(key: String, value: LispExp.Expression): Env = newFrame withValue (key, value)
    def withValues(context: Seq[(String, LispExp.Expression)]): Env = newFrame withValues context
    def newMutableFrame = MutableEnv(MutableMap.empty, this)
    def directHas(key: String) = false
    def forceUpdated(key: String, value: LispExp.Expression): Environment = this
    def isMutable(key: String) = false
    def collectBy(p: Environment => Boolean): Environment = this
    def isEmpty: Boolean = this == EmptyEnv
    def collectDefinedValues: Set[String] = Set.empty
    def flatten: Environment = {
      val values = collectDefinedValues
      if(values.isEmpty) EmptyEnv
      else {
        val (mutable, immutable) = values.partition(isMutable)
        val immutableParts = immutable.zip(immutable.map(x => getValueOption(x).get)).toMap
        val mutableParts = MutableMap.from(mutable.zip(mutable.map(x => getValueOption(x).get)))
        if (mutable.isEmpty)
          Env(immutableParts, EmptyEnv)
        else if(immutable.isEmpty)
          MutableEnv(mutableParts, EmptyEnv)
        else Env(immutableParts, MutableEnv(mutableParts, EmptyEnv))
      }
    }
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
      else copy(parent=parent.forceUpdated(key, value))

    override def isMutable(key: String): Boolean = !directHas(key) && parent.isMutable(key)

    override def collectBy(p: Environment => Boolean): Environment =
      if (!p(this)) EmptyEnv
      else copy(parent=parent.collectBy(p))

    override def isEmpty: Boolean = env.isEmpty && parent.isEmpty

    override def collectDefinedValues: Set[String] = env.keySet ++ parent.collectDefinedValues
  }
  case object EmptyEnv extends Environment {
    override def has(key: String): Boolean = false

    override def getValueOption(key: String): Option[LispExp.Expression] = None

    override def toString: String = "{}"

    override def isEmpty: Boolean = true
  }

  case class CombineEnv(env: Seq[Environment]) extends Environment {
    override def has(key: String): Boolean = env.exists(_ has key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      env.find(_ has key).flatMap(_ getValueOption key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment = {
      val idx = env.indexWhere(_ has key)
      if (idx < 0) this
      else {
        val toUpdate = env(idx)
        copy(env.updated(idx, toUpdate.forceUpdated(key, value)))
      }
    }

    override def isMutable(key: String): Boolean = {
      env.find(_ has key).exists(_ isMutable key)
    }

    override def collectBy(p: Environment => Boolean): Environment = {
      val flatSeq = env.filter(p).map(_.collectBy(p)).filter(_ != EmptyEnv)
//      println(s"$env flatten to $flatSeq")
      if (flatSeq.isEmpty) EmptyEnv
      else copy(flatSeq)
    }

    override def isEmpty: Boolean = env.forall(_.isEmpty)

    override def collectDefinedValues: Set[String] =
      env.map(_.collectDefinedValues).reduce(_ ++ _)
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
      else if(has(key)) copy(parent=parent.forceUpdated(key, value))
      else this

    override def isMutable(key: String): Boolean = directHas(key) || parent.isMutable(key)

    override def isEmpty: Boolean = env.isEmpty && parent.isEmpty

    override def collectBy(p: Environment => Boolean): Environment =
      if (!p(this)) EmptyEnv
      else copy(parent=parent.collectBy(p))

    override def collectDefinedValues: Set[String] =
      env.keySet.toSet ++ parent.collectDefinedValues
  }

  object MutableEnv {
    def createEmpty = MutableEnv(MutableMap.empty, EmptyEnv)
  }

  case class NameSpacedEnv(nameSpace: String, env: Environment, separator: String = ".") extends Environment {
    private val prefix = if (nameSpace.isEmpty) "" else s"$nameSpace$separator"
    private val prefixLength = prefix.length
    private def stripHead(key: String) =
      if (key.startsWith(prefix)) key.substring(prefixLength) else ""
    override def has(key: String): Boolean = key.startsWith(prefix) && env.has(key.substring(prefixLength))

    override def getValueOption(key: String): Option[LispExp.Expression] =
      if (has(key)) env.getValueOption(key.substring(prefixLength))
      else None

    override def directHas(key: String): Boolean =
      has(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      if(key.startsWith(prefix)) copy(env=env.forceUpdated(stripHead(key), value)) else this

    override def isMutable(key: String): Boolean =
      has(key) && env.isMutable(stripHead(key))

    override def collectBy(p: Environment => Boolean): Environment = {
      val newEnv = env.collectBy(p)
      if (newEnv.isEmpty) EmptyEnv
      else copy(env=newEnv)
    }

    override def collectDefinedValues: Set[String] = env.collectDefinedValues.map(prefix.concat)
  }

  case class TransparentLayer(layer: Environment, base: Environment) extends Environment {
    override def has(key: String): Boolean = layer.has(key) || base.has(key)

    override def getValueOption(key: String): Option[LispExp.Expression] =
      layer.getValueOption(key).orElse(base.getValueOption(key))

    override def directHas(key: String): Boolean = base.directHas(key)

    override def forceUpdated(key: String, value: LispExp.Expression): Environment =
      if(layer.has(key)) copy(layer=layer.forceUpdated(key, value))
      else if(base.has(key)) copy(base=base.forceUpdated(key, value))
      else this

    override def isMutable(key: String): Boolean =
      layer.isMutable(key) || base.isMutable(key)

    override def collectBy(p: Environment => Boolean): Environment = {
      val newLayer = layer.collectBy(p)
      if (newLayer.isEmpty) base.collectBy(p)
      else {
        val baseLayer = base.collectBy(p)
        if (baseLayer.isEmpty) newLayer
        else copy(newLayer, baseLayer)
      }

    }

    override def collectDefinedValues: Set[String] =
      layer.collectDefinedValues ++ base.collectDefinedValues
  }
}
