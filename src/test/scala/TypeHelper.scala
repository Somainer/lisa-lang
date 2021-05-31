import moe.roselia.lisa.Typing.LisaType
import org.scalatest.OptionValues

trait TypeHelper extends OptionValues {
  implicit def lisaTypeIsOrdered[T <: LisaType]: Ordering[T] =
    _.tryCompareTo(_).value
}
