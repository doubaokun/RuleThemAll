package sta.common

import scala.language.dynamics

object Reflect {
  class ReflectOps(val obj: Any) extends AnyVal with Dynamic {
    def applyDynamic(name: String)(args: Any*) = {
      obj.getClass.getMethod(name, args.map(_.getClass): _*).invoke(obj, args.map(_.asInstanceOf[AnyRef]): _*)
    }
  }

  implicit class ToReflectOps(val obj: Any) extends AnyVal {
    def reflect = new ReflectOps(obj)
  }
}
