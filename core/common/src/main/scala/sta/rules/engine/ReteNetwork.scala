//package sta.rules.engine
//
//import scala.language.existentials
//
//import sta.rules.engine.condition.Condition
//
//class ReteNetwork extends Network {
//  private val alphaNetwork = new AlphaNetwork
//
//  def addWME(wme: WME) = {
//    alphaNetwork += wme
//
//    this
//  }
//
//  def addProduction(production: Production): this.type = {
//    //    val key = extractor.extract(production)
//    production.foreach { rule =>
//      alphaNetwork += new AlphaMemory[Condition[_]] {
//        def objectTest: Condition[_] = ???
//
//        def valueTest: Condition[_] = ???
//
//        def attributeTest: Condition[_] = ???
//
//        def successors: Seq[JoinNode] = ???
//      }
//    }
//
//    this
//  }
//
//  def -(production: Production): this.type = {
//    this
//  }
//}
