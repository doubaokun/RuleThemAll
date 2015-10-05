package rta.app

import android.view.{View, ViewGroup}
import android.widget.{AbsListView, AdapterView, BaseAdapter}
import macroid.FullDsl._
import macroid._
import macroid.util.SafeCast
import macroid.viewable.Listable
import scala.collection.{GenSet, mutable}
import rta.logging.Logging

class ListableStableAdapter[T <: AnyRef, W <: View](data: Set[T])(implicit ctx: ActivityContext,
  appCtx: AppContext, listable: ListableViewModifier[T, W]) extends BaseAdapter with Logging {
  private[this] val mapping = mutable.LongMap.empty[T]
  private[this] var seq = Vector.empty[T]

  swap(data)

  def swap(other: GenSet[T]): Unit = {
    val unique = seq.toSet

    other.diff(unique).foreach { e =>
      mapping += (e.hashCode().toLong, e)
      seq +:= e
    }

    val removed = unique.diff(other)
    removed.foreach(mapping -= _.hashCode().toLong)
    seq = seq.filterNot(removed.contains)

    notifyDataSetChanged()
  }

  def apply(id: Long) = mapping(id)

  def clear(): Unit = {
    mapping.clear()
    seq = Vector.empty

    notifyDataSetChanged()
  }

  def getCount: Int = seq.length

  def getItemId(position: Int): Long = if (0 <= position && position < getCount) {
    seq(position).hashCode().toLong
  } else {
    position.toLong
  }

  def getItem(position: Int): AnyRef = if (0 <= position && position < getCount) {
    seq(position)
  } else {
    null
  }

  @SuppressWarnings(Array("org.brianmckenna.wartremover.warts.OptionPartial"))
  def getView(position: Int, view: View, parent: ViewGroup): View = getUi {
    val p = SafeCast[ViewGroup, AbsListView](parent).get
    val v = SafeCast[View, W](view).map(x => Ui(x))
      .getOrElse(listable.makeView(getItemViewType(position)))
    listable.fillView(listable.modifyView(p, position, v), apply(getItemId(position)))
  }

  override def getViewTypeCount = listable.viewTypeCount

  override def getItemViewType(position: Int) = if (0 <= position && position < getCount) {
    listable.viewType(apply(getItemId(position)))
  } else {
    super.getItemViewType(position)
  }

  override def hasStableIds: Boolean = true
}
