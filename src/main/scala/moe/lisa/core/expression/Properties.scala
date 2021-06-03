package moe.lisa.core.expression

import dotty.tools.dotc.util.Property._
import dotty.tools.dotc.util.Attachment
import moe.lisa.core.Name
import moe.lisa.lang.{Atom, IRecord, LisaRecord}
import moe.lisa.runtime.constructs.LisaObjects

object Properties {
  val BackQuoted = new StickyKey[Unit]
  val DocString = new StickyKey[String]
  val Macro = new StickyKey[Unit]
  val QuasiSplicing = new StickyKey[Unit]
  val MetaData = new StickyKey[IRecord]

  val unitProperties: Set[Key[?]] = Set(BackQuoted, Macro, QuasiSplicing)

  val metaTransform: Map[String, StickyKey[?]] = Map(
    "macro" -> Macro,
    "doc" -> DocString,
  )
  val propertyToMeta: Map[StickyKey[?], String] = metaTransform.map((k, v) => (v, k))

  enum BracketKind:
    case Round
    case Square
    
    def isRound: Boolean = this == BracketKind.Round
    def isSquare: Boolean = this == BracketKind.Square
    
  
  val BracketType = new StickyKey[BracketKind]

  extension[T <: Attachment.Container](container: T)
    def withAttachment[K](key: Key[K], value: K): T =
      container.putAttachment(key, value)
      container
    def addMarker(key: Key[Unit]): T = container.withAttachment(key, ())

    def toMetaRecord: LisaRecord =
      val map: Map[Any, Any] = propertyToMeta.keys.flatMap { key =>
        val metaKey = propertyToMeta(key)
        val metaAtom = LisaObjects.atom(metaKey)
        val attachment = container.getAttachment(key)
        val metaValue =
          if unitProperties(key) then attachment.orElse(Some(false))
          else attachment
        attachment.map(att => (metaAtom, att))
      }.toMap

      val metaData = container.getAttachment(MetaData).getOrElse(LisaRecord.empty)

      val record = metaData.updatedMany(LisaRecord(map))
      LisaRecord.fromIRecord(record)

    def restoreMeta(meta: IRecord): Unit =
      for case atm @ Atom(name) <- meta.keys do
        val value = meta.lookup(atm)
        for key <- metaTransform.get(name) do
          if unitProperties(key) then
            value match {
              case true => container.putAttachment(key, ())
              case false => container.removeAttachment(key)
              case _ => throw new IllegalArgumentException(s"$name should be a bool, but $value found.")
            }
          else
            container.putAttachment(key, value)
      container.putAttachment(MetaData, meta)
}
