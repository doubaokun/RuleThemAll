package sta.app

import android.view.{View, ViewGroup}
import android.widget.BaseAdapter
import java.util.concurrent.atomic.AtomicLong
import kj.android.logging.Logging
import macroid.FullDsl._
import macroid._
import macroid.util.SafeCast
import macroid.viewable.Listable
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.{GenSet, mutable}
import scala.util.control.NonFatal

class ListableStableAdapter[T <: AnyRef, W <: View](data: Set[T])(implicit ctx: ActivityContext,
  appCtx: AppContext, listable: Listable[T, W]) extends BaseAdapter with Logging {
  private[this] val nextId = new AtomicLong(0)
  private[this] val mapping = mutable.LongMap.empty[T]
  private[this] val inverseMapping = mutable.Map.empty[T, Long]
  private[this] var seq = mutable.ArrayBuffer.empty[T]

  swap(data)

  def swap(other: GenSet[T]): Unit = {
    log.info(s"Before $seq $mapping $inverseMapping")
    val unique = seq.toSet

    other.diff(unique).foreach { e =>
      val id = nextId.getAndIncrement()
      mapping += (id, e)
      inverseMapping += (e -> id)
      seq += e
    }

    val removed = unique.diff(other)
    removed.foreach { e =>
      val id = inverseMapping(e)
      mapping -= id
      inverseMapping -= e
    }
    seq = seq.filterNot(removed.contains)

    log.info(s"After $seq $mapping $inverseMapping")
    notifyDataSetChanged()
  }

  def apply(id: Long) = mapping(id)

  def clear(): Unit = {
    mapping.clear()
    inverseMapping.clear()
    seq = ArrayBuffer.empty

    notifyDataSetChanged()
  }

  def getCount: Int = mapping.size

  def getItemId(position: Int): Long = try {
    inverseMapping(seq(position))
  } catch {
    case NonFatal(th) =>
      log.error("Error has occurred", th)
      position.toLong
  }

  def getItem(position: Int): AnyRef = mapping(getItemId(position))

  def getView(position: Int, view: View, parent: ViewGroup): View = getUi {
    val v = SafeCast[View, W](view).map(x => Ui(x))
      .getOrElse(listable.makeView(getItemViewType(position)))
    listable.fillView(v, apply(getItemId(position)))
  }

  override def getViewTypeCount = listable.viewTypeCount

  override def getItemViewType(position: Int) = if (0 <= position && position < getCount) {
    listable.viewType(apply(getItemId(position)))
  } else {
    super.getItemViewType(position)
  }

  override def hasStableIds: Boolean = true
}
