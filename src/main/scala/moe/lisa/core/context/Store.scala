package moe.lisa.core.context

import scala.compiletime.erasedValue

class Store(private val elems: Array[Any]) extends AnyVal:
  import Store._

  def newLocation[T](): (Location[T], Store) =
    val el = Array.copyOf(elems, elems.length + 1)
    (location(elems.length), Store(el))

  def newLocation[T](init: T): (Location[T], Store) =
    val (loc, store) = newLocation[T]()
    store.elems(loc.asInstanceOf[Int]) = init
    (loc, store)

  inline def newLocations[T <: Tuple]: (Tuple.Map[T, Location], Store) =
    inline erasedValue[T] match
      case _: EmptyTuple => (EmptyTuple.asInstanceOf[Tuple.Map[T, Location]], this)
      case _: (t *: ts) =>
        val (loc, store) = newLocation[t]()
        val (locs, newStore) = store.newLocations[ts]
        ((loc *: locs).asInstanceOf[Tuple.Map[T, Location]], newStore)

  def updated[T](loc: Location[T], value: T): Store =
    val el = elems.clone()
    el(loc.asInstanceOf[Int]) = value
    Store(el)

  def apply[T](loc: Location[T]): T =
    elems(loc.asInstanceOf[Int]).asInstanceOf[T]

object Store:
  opaque type Location[T] = Int

  val empty: Store = Store(Array())
  private def location[T](i: Int): Location[T] = i
