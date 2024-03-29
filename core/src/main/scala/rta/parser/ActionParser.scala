package rta.parser

import fastparse.noApi._
import scala.reflect.{ClassTag, classTag}
import rta.model.actions.Action

abstract class ActionParser[A <: Action: ClassTag] extends BasicRules with Extras with Serializable {
  def actionClass: Class[_] = classTag[A].runtimeClass

  def Rule: P[A]
}
