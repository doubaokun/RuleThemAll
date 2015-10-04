package sta.app

import android.view.View
import android.widget.AbsListView
import macroid.{Ui, AppContext, ActivityContext}
import macroid.viewable.Listable

trait ListableViewModifier[T, W <: View] extends Listable[T, W] {
  def modifyView(parent: AbsListView, position: Int, view: Ui[W]): Ui[W]
}

object ListableViewModifier {
  def fromListable[T, W <: View](listable: Listable[T, W])
    (modifier: (AbsListView, Int, Ui[W]) => Ui[W]): ListableViewModifier[T, W] = new ListableViewModifier[T, W] {
    def modifyView(parent: AbsListView, position: Int, view: Ui[W]): Ui[W] =
      modifier(parent, position, view)

    def fillView(view: Ui[W], data: T)(implicit ctx: ActivityContext, appCtx: AppContext): Ui[W] =
      listable.fillView(view, data)

    def viewType(data: T): Int = listable.viewType(data)

    def viewTypeCount: Int = listable.viewTypeCount

    def makeView(viewType: Int)(implicit ctx: ActivityContext, appCtx: AppContext): Ui[W] =
      listable.makeView(viewType)
  }
}
