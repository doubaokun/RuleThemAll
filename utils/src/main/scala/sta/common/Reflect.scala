package sta.common

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Reflect {
  class ReflectOps[T, U](val obj: T) extends AnyVal with Dynamic {
    def applyDynamic(name: String)(args: Any*): U = macro ReflectMacros.reflectMethod[T, U]

    def selectDynamic(name: String): U = macro ReflectMacros.reflectField[T, U]
  }

  class ReflectStaticOps[T, U](val clazz: Class[T]) extends AnyVal with Dynamic {
    def applyDynamic(name: String)(args: Any*): U = macro ReflectMacros.reflectStaticMethod[T, U]
    
    def selectDynamic(name: String): U = macro ReflectMacros.reflectStaticField[T, U]
  }

  implicit class ToReflectOps[T](val clazz: Class[T]) extends AnyVal {
    def reflect[U](self: T) = new ReflectOps[T, U](self)

    def reflect[U] = new ReflectStaticOps[T, U](clazz)
  }
}

private class ReflectMacros(val c: blackbox.Context) {
  import c.universe._

  private def getThis(prefix: Tree, objType: Type, retType: Type): Tree = {
    prefix match {
      case q"""$_[..${List(tptT)}](...${List(List(Literal(Constant(`objType`))))}).reflect[..${List(tptU)}](...${List(List(self))})"""
        if tptT.tpe =:= objType && tptU.tpe =:= retType  =>
        self
      case _ => c.abort(c.enclosingPosition, s"Invalid prefix tree: $prefix")
    }
  }

  private def reflectMethod(obj: Tree, objType: Type, retType: Type, name: c.Expr[String], args: c.Expr[Any]*): Tree = {
    val classes = args.map(arg => q"classOf[${arg.actualType}]")
    val casted = args.map(arg => q"$arg.asInstanceOf[AnyRef]")

    q"""{
          val m = classOf[$objType].getDeclaredMethod($name, ..$classes)
          m.setAccessible(true)
          m.invoke($obj, ..$casted).asInstanceOf[$retType]
        }
     """
  }

  private def reflectField(obj: Tree, objType: Type, retType: Type, name: c.Expr[String]): Tree = {
    q"""{
          val f = classOf[$objType].getDeclaredField($name)
          f.setAccessible(true)
          f.get($obj).asInstanceOf[$retType]
        }
     """
  }

  def reflectMethod[T: WeakTypeTag, U: WeakTypeTag](name: c.Expr[String])(args: c.Expr[Any]*): Tree = {
    val tpeT = weakTypeOf[T]
    val tpeU = weakTypeOf[U]

    reflectMethod(getThis(c.prefix.tree, tpeT, tpeU), tpeT, tpeU, name, args: _*)
  }

  def reflectStaticMethod[T: WeakTypeTag, U: WeakTypeTag](name: c.Expr[String])(args: c.Expr[Any]*): Tree = {
    reflectMethod(q"null", weakTypeOf[T], weakTypeOf[U], name, args: _*)
  }

  def reflectField[T: WeakTypeTag, U: WeakTypeTag](name: c.Expr[String]): Tree = {
    val tpeT = weakTypeOf[T]
    val tpeU = weakTypeOf[U]

    reflectField(getThis(c.prefix.tree, tpeT, tpeU), tpeT, tpeU, name)
  }

  def reflectStaticField[T: WeakTypeTag, U: WeakTypeTag](name: c.Expr[String]): Tree = {
    reflectField(q"null", weakTypeOf[T], weakTypeOf[U], name)
  }
}
