package rta.service

import org.scalatest.{Matchers, FlatSpec}
import rta.model.triggers.Trigger.Branch
import rta.model.{Rule, ModelHelpers}
import rta.model.triggers.{Trigger, TriggerSpec}
import rta.tests.PropertyChecks
import scala.collection.mutable
import spire.implicits._
import spire.math.UByte

class ExecutionNetworkSpec extends FlatSpec with PropertyChecks with Matchers with ModelHelpers {
  behavior of "Network"
  
  trait Instances {
    def m1 = Trigger.Condition[TestModel](_.i != 1).named("m1")
    def m2 = Trigger.Condition[TestModel](_.o == TestModel.O.Object1.asInstanceOf[TestModel.O]).named("m2")
    def m3 = Trigger.Condition[TestModel](_.s == "ala ma kota").named("m3")
    def m4 = Trigger.Condition[TestModel](_.i > 1).named("m4")
    def m5 = Trigger.Condition[TestModel](_.s != "ala ma kota").named("m5")
    def m6 = Trigger.Condition[TestModel](_.i < 1).named("m6")
    def m7 = Trigger.Condition[TestModel](_.o == TestModel.O.Object2.asInstanceOf[TestModel.O]).named("m7")
  }
//
//  Set(
//    Alpha(m4,Set(Alpha(m1,Set()), Alpha(m5,Set()))),
//    Alpha(m1,Set(Alpha(m4,Set()), Terminal(rule4), Alpha(m2,Set()), Terminal(rule1), Terminal(rule2))),
//    Alpha(m2,Set(Alpha(m4,Set()), Alpha(m3,Set()))),
//    Alpha(m6,Set(Alpha(m7,Set())))
//  )




  it should "" in new Instances {
    import rta.common.Reflect._

    val rule1 = Rule("rule1", UByte(127), actions = Seq.empty, branches = Seq(
      Branch(timers = Nil, conditions = Seq(m1, m2, m3)),
      Branch(timers = Nil, conditions = Seq(m4, m1))
    ))
    val rule2 = Rule("rule2", UByte(127), actions = Seq.empty, branches = Seq(
      Branch(timers = Nil, conditions = Seq(m1, m2, m3)),
      Branch(timers = Nil, conditions = Seq(m4, m1))
    ))
    val rule3 = Rule("rule3", UByte(127), actions = Seq.empty, branches = Seq(
      Branch(timers = Nil, conditions = Seq(m1, m4, m5, m7))
    ))
    val rule4 = Rule("rule4", UByte(127), actions = Seq.empty, branches = Seq(
      Branch(timers = Nil, conditions = Seq(m2, m4, m1)),
      Branch(timers = Nil, conditions = Seq(m6, m7))
    ))

    val network = ExecutionNetwork(Seq(rule1, rule2, rule3, rule4).iterator)
    println(classOf[ExecutionNetwork].reflect[AnyRef](network).`rta$service$ReteNetwork$$network`)
    println(classOf[ExecutionNetwork].reflect[AnyRef](network).`rta$service$ReteNetwork$$rules`)

    val set1 = network.activate(TestModel(0, "ala ma kota", TestModel.O.Object2)) // m1, m3, m6, m7
    set1.map(_.name) should contain theSameElementsAs Seq(rule4.name)

    val set2 = network.activate(TestModel(0, "ala ma kota", TestModel.O.Object1)) // m1, m2, m3, m6
    set2.map(_.name) should contain theSameElementsAs Seq(rule1.name, rule2.name)

    val set3 = network.activate(TestModel(2, "ala ma kota", TestModel.O.Object1)) // m1, m4, m5, m7
    set3.map(_.name) should contain theSameElementsAs Seq(rule1.name, rule2.name, rule3.name)
  }
}
