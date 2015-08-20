package sta.common

import org.robolectric.annotation.Config
import org.scalatest.{RobolectricSuite, Matchers, WordSpec}

@Config(sdk = Array(21), manifest = Config.NONE)
class UsedFeaturesSpec extends WordSpec with RobolectricSuite with Matchers {
  def usedFeatures[T: UsedFeatures] = implicitly[UsedFeatures[T]]

  @category("base")
  sealed abstract class Base

  case class Impl1() extends Base

  @feature("a", "b")
  @action("A")
  object Impl2 extends Base

  @category("baseImpl")
  @feature("c")
  @action("B")
  sealed abstract class BaseImpl extends Base

  @category("impl3")
  class Impl3 extends BaseImpl

  @feature("d")
  @action("C")
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
        usedFeatures[Base].intents.map(_.getAction) should ===(Set("A", "B"))
        usedFeatures[Impl2.type].intents.map(_.getAction) should ===(Set("A"))
        usedFeatures[BaseImpl].intents.map(_.getAction) should ===(Set("B", "C"))
        usedFeatures[Impl4.type].intents.map(_.getAction) should ===(Set("C"))
      }
    }
  }
}
