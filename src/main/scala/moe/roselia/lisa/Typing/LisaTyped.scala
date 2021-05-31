package moe.roselia.lisa.Typing

import moe.roselia.lisa.LispExp.Expression

trait LisaTyped { self: Expression =>
  def lisaType: LisaType
}

trait StaticTyped extends LisaTyped { self: Expression =>}

trait InferrableTyped extends LisaTyped { this: Expression =>
  private var typing: LisaType = Untyped
  def withType(tpe: LisaType): this.type = {
    typing = tpe
    this
  }

  override def lisaType: LisaType = typing
}
