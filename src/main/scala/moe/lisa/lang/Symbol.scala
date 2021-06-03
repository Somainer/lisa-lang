package moe.lisa.lang

case class Symbol(fullName: String) extends INamed:
  private val (ns, namePart) = {
    fullName.indexOf('/') match
      case -1 => (None, fullName)
      case index =>
        (Some(fullName.substring(0, index)), fullName.substring(index + 1))
  }

  override def namespace: String = ns.getOrElse("")
  override def name: String = namePart
