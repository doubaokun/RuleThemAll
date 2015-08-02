package kj.android.common

import scala.reflect.macros.blackbox

trait MacrosHelpers {
  protected def allAnnotationsFor(ctx: blackbox.Context)(tpe: ctx.universe.Type): List[ctx.universe.Annotation] = {
    import ctx.universe._

    val builder = List.newBuilder[Annotation]
    builder ++= tpe.baseClasses.flatMap { sym =>
      if (sym.companion != NoSymbol) sym.annotations ++ sym.companion.annotations
      else sym.annotations
    }
    builder.result()
  }

  protected def mapChildren[T](ctx: blackbox.Context)(of: ctx.universe.Tree)(f: ctx.universe.Type => T): List[T] = {
    import ctx.universe._

    val baseTpe = ctx.typecheck(q"this").tpe.typeSymbol.typeSignature
    val predicateSym = ctx.typecheck(q"type T = $of").symbol.typeSignature.typeSymbol // FIXME looks like a workaround
    baseTpe.asInstanceOf[ClassInfoTypeApi].parents.collect {
      case tpe if tpe.baseClasses.contains(predicateSym) => f(tpe)
    }
  }
}
