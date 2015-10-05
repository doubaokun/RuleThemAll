package rta.parser

import scala.language.experimental.macros
import scala.language.implicitConversions
import fastparse.noApi._
import scala.reflect.macros.blackbox
import rta.model.BaseModel
import rta.model.triggers.Trigger

abstract class TriggerParser[M <: BaseModel] extends BasicRules with Extras with Serializable {
  def matchStringParser(extractor: M => String): P[Trigger.Condition[M]] = macro TriggerParserMacros.matchStringParser[M]

  def Prefix: String

  def Main: P[Trigger.Standalone[_ <: M]]

  lazy val Rule: P[Trigger] = Main
}

private[parser] class TriggerParserMacros(val c: blackbox.Context) {
  import c.universe._
  
  def ConditionTrigger(tpe: Type) = appliedType(typeOf[Trigger.Condition[_]].typeConstructor, tpe)

  def matchStringParser[M <: BaseModel: WeakTypeTag](extractor: c.Expr[M => String]): Tree = {
    val tpe = weakTypeOf[M]

    q"""fastparse.noApi.P(
          ("contains".withWS ~ String map (str => new ${ConditionTrigger(tpe)}($extractor(_).contains(str)))) |
          ("matches".withWS ~ String map { str =>
            val regex = str.r
            new ${ConditionTrigger(tpe)}(m => regex.findFirstIn($extractor(m)).isDefined)
          })
        )
     """
  }
}
