package moe.lisa.lang

import moe.lisa.lang

trait IRecord extends ILookupable, IDynamicInvokable:
  type Self <: IRecord
  override def apply(arg: Any): Any = lookup(arg)
  override def apply(arg1: Any, arg2: Any): Any = lookup(arg1, arg2)

  def updated(key: Any, value: Any): Self
  def removed(key: Any): Self

  def keys: Iterable[Any]
  def updatedMany(that: IRecord): Self =
    that.keys.foldLeft(this) { case (r, k) =>
      r.updated(k, that.lookup(k))
    }.asInstanceOf[Self]

trait IMapRecord extends IRecord:
  override type Self <: IMapRecord
  def map: Map[Any, Any]

  override def keys: Iterable[Any] = map.keys
  override def lookupOption(v: Any): Option[Any] = map.get(v)

case class LisaRecord(map: Map[Any, Any]) extends IMapRecord:
  override type Self = LisaRecord

  override def updated(key: Any, value: Any): LisaRecord =
    copy(map.updated(key, value))

  override def removed(key: Any): LisaRecord =
    copy(map.removed(key))

  override def updatedMany(that: IRecord): LisaRecord = that match
    case LisaRecord(m) => LisaRecord(map ++ m)
    case _ => super.updatedMany(that)

object LisaRecord:
  val empty = LisaRecord(Map.empty)

  def fromIRecord(record: IRecord): LisaRecord = record match {
    case lr: LisaRecord => lr
    case r =>
      val mapBuilder = collection.Map.newBuilder[Any, Any]
      for key <- r.keys do
        mapBuilder.addOne((key, r.lookup(key)))
      LisaRecord(mapBuilder.result().toMap)
  }
