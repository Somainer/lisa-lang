package moe.roselia.lisa.Typing

case class NameOnlyType(name: String) extends AnyType {
  override def fullName: String = name

  override def isAssignableTo(other: LisaType): Option[Boolean] = None
}
