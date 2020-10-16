package moe.roselia.lisa.Import

import moe.roselia.lisa.Environments.Environment
import moe.roselia.lisa.LispExp.{Expression, LisaMutableRecord}

object EnvironmentWrapper {
  def wrapEnvironment(env: Environment, name: String): LisaMutableRecord[Expression] = new LisaMutableRecord[Expression] {
    override def updateValue(key: String, value: Expression): Unit = env.forceUpdated(key, value)

    override def selectDynamic(key: String): Expression = env.get(key)

    override def containsKey(key: String): Boolean = env has key

    override def getOrElse[EV >: Expression](key: String, otherwise: => EV): EV = env.getValueOption(key).getOrElse(otherwise)

    override def recordTypeName: String = name

    override def indented(bySpace: Int, level: Int): String = toString

    override def toString: String = s"$name { ... }"
  }
}
