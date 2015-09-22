package sta.parser

import scala.language.experimental.macros
import scala.language.implicitConversions
import fastparse.noApi._
import scala.reflect.macros.blackbox
import sta.model.BaseModel
import sta.model.triggers.Trigger

abstract class TriggerParser[M <: BaseModel] extends BasicRules with Extras with Serializable {
  def matchStringParser(extractor: M => String): P[Trigger.Condition[M]] = macro TriggerParserMacros.matchStringParser[M]

  def Prefix: String

  def Main: P[Trigger.Standalone[_ <: M]]

  lazy val Rule: P[Trigger] = Main

  override final def equals(o: Any): Boolean = o match {
    case p: TriggerParser[_] => p.Prefix.split("\\s+").mkString(" ") == Prefix.split("\\s+").mkString(" ")
    case _ => false
  }

  override final def hashCode(): Int = Prefix.split("\\s+").mkString(" ").hashCode
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
