package sta.common

import org.scalatest.{Matchers, WordSpec}

class UsedFeaturesSpec extends WordSpec with Matchers {
  def usedFeatures[T: UsedFeatures] = implicitly[UsedFeatures[T]]

  @category("base")
  sealed abstract class Base

  case class Impl1() extends Base

  @feature("a", "b")
  @intent("A", "B")
  object Impl2 extends Base

  @category("baseImpl")
  @feature("c")
  @intent("C")
  sealed abstract class BaseImpl extends Base

  @category("impl3")
  class Impl3 extends BaseImpl

  @feature("d")
  @intent("D")
  case object Impl4 extends BaseImpl

  "UsedFeatures" when {
    "dealing with categories" should {
      "return the most specific one" in {
        usedFeatures[Base].category should ===("base")
        usedFeatures[Impl1].category should ===("base")
        usedFeatures[Impl3].category should ===("impl3")
        usedFeatures[Impl4.type].category should ===("baseImpl")
      }
    }

    "dealing with features" should {
      "collect all from direct children" in {
        usedFeatures[Base].features should ===(Set("a", "b", "c"))
        usedFeatures[Impl2.type].features should ===(Set("a", "b"))
        usedFeatures[BaseImpl].features should ===(Set("c", "d"))
        usedFeatures[Impl4.type].features should ===(Set("d"))
      }
    }

    "dealing with intents" should {
      "collect all from direct children" in {
        usedFeatures[Base].intents should ===(Set("A", "B", "C"))
        usedFeatures[Impl2.type].intents should ===(Set("A", "B"))
        usedFeatures[BaseImpl].intents should ===(Set("C", "D"))
        usedFeatures[Impl4.type].intents should ===(Set("D"))
      }
    }
  }
}
