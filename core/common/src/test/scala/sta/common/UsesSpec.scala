package sta.common

import android.content.Intent
import org.robolectric.annotation.Config
import org.scalatest.{RobolectricSuite, Matchers, WordSpec}
import Uses._

@Config(sdk = Array(21), manifest = Config.NONE)
class UsesSpec extends WordSpec with RobolectricSuite with Matchers {
  def usedFeatures[T: Uses] = implicitly[Uses[T]]

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

  "Uses" when {
    "dealing with categories" should {
      "return the most specific one" in {
        categoryOf[Base] should ===("base")
        categoryOf[Impl1] should ===("base")
        categoryOf[Impl3] should ===("impl3")
        categoryOf[Impl4.type] should ===("baseImpl")
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
        usedFeatures[Base].requirements should === (Set("A", "B").map(a =>
          new Requirement.IntentBased(new Intent(a))
        ))
        usedFeatures[Impl2.type].requirements should === (Set("A").map(a =>
          new Requirement.IntentBased(new Intent(a))
        ))
        usedFeatures[BaseImpl].requirements should === (Set("B", "C").map(a =>
          new Requirement.IntentBased(new Intent(a))
        ))
        usedFeatures[Impl4.type].requirements should === (Set("C").map(a =>
          new Requirement.IntentBased(new Intent(a))
        ))
      }
    }
  }
}
