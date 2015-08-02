//package sta.parser
//
//import scala.language.experimental.macros
//
//import kj.android.common.MacrosHelpers
//import org.parboiled2.Rule1
//import scala.reflect.macros.blackbox
//import sta.model.actions.Action
//
//object RuleMacros {
//  def MainA: Rule1[Action] = macro RuleMacrosImpl.MainA
//
//  def MainT: Rule1[sta.model.triggers.Trigger] = macro RuleMacrosImpl.MainT
//}
//
//// TODO cache super call in MainA, MainT, prefixes ???
//private class RuleMacrosImpl(val c: blackbox.Context) extends MacrosHelpers {
//
//  import c.universe._
//
//  private def superSelector(tpe: Type): TermName => Tree =
//    superMethod => q"super[${tpe.typeSymbol.name.toTypeName}].$superMethod"
//
//  def MainA: c.Expr[Rule1[Action]] = {
//    val rules = {
//      val selected = mapChildren(c)(tq"sta.parser.actions.ActionParserPart")(superSelector)
//      require(selected.nonEmpty, s"${c.typecheck(q"this").tpe.typeSymbol.name.decodedName.toString}" +
//        " must mix at least one descendant of sta.parser.actions.ActionParserPart")
//
//      selected.tail.foldLeft(selected.head(TermName("MainA"))) {
//        case (acc, t) => q"$acc | ${t(TermName("MainA"))}"
//      }
//    }
//
//    c.Expr(q"rule($rules)")
//  }
//
//  def MainT: c.Expr[Rule1[sta.model.triggers.Trigger]] = {
//    def twoOrMore(of: Tree) = q"'(' ~ $of ~ ',' ~ oneOrMore($of).separatedBy(',') ~ ')'"
//
//    val rules = {
//      val tpp = tq"sta.parser.triggers.TriggerParserPart"
//      val children = mapChildren(c)(tpp) { parserTpe =>
//        val selected = superSelector(parserTpe)
//        val category = selected(TermName("prefix"))
//
//        (selected, category)
//      }
//      if (children.isEmpty) c.abort(
//        c.enclosingPosition,
//        s"${c.typecheck(q"this").tpe.typeSymbol.name.decodedName.toString}" +
//          " must mix at least one descendant of sta.parser.triggers.TriggerParserPart"
//      )
//
//      def single(t: (TermName => c.Tree, c.Tree)): Tree = {
//        val prfx = t._2
//        val main = t._1(TermName("MainT"))
//        val main2 = q"'(' ~ $main ~ ',' ~ $main ~ ')'"
//        val main2toN = twoOrMore(q"$main")
//        q"""($prfx ~ (('(' ~ (
//              (oneOrMore($main).separatedBy(',') ~> (sta.model.triggers.Trigger(_))) |
//              ("or" ~ $main2toN ~> ((t: sta.model.triggers.Trigger, ts: scala.collection.immutable.Seq[sta.model.triggers.Trigger]) =>
//                sta.model.triggers.Trigger.or(t +: ts))) |
//              ("xor" ~ $main2 ~> ((t1: sta.model.triggers.Trigger, t2: sta.model.triggers.Trigger) =>
//                sta.model.triggers.XorTrigger(t1, t2))) |
//              ("and" ~ $main2toN ~> ((t: sta.model.triggers.Trigger, ts: scala.collection.immutable.Seq[sta.model.triggers.Trigger]) =>
//                sta.model.triggers.Trigger.and(t +: ts)))
//           ) ~ ')') | $main))"""
//      }
//
//      children.tail.foldLeft(single(children.head)) {
//        case (acc, t) => q"$acc | ${single(t)}"
//      }
//    }
//    val rules2 = q"'(' ~ $rules ~ ',' ~ $rules ~ ')'"
//    val rules2toN = twoOrMore(q"$rules")
//
//    c.Expr(
//      q"""rule(
//            (oneOrMore($rules).separatedBy(',') ~> (sta.model.triggers.Trigger(_))) |
//            ("or" ~ $rules2toN ~> ((t: sta.model.triggers.Trigger, ts: scala.collection.immutable.Seq[sta.model.triggers.Trigger]) =>
//              sta.model.triggers.Trigger.or(t +: ts))) |
//            ("xor" ~ $rules2 ~> ((t1: sta.model.triggers.Trigger, t2: sta.model.triggers.Trigger) =>
//              sta.model.triggers.XorTrigger(t1, t2))) |
//            ("and" ~ $rules2toN ~> ((t: sta.model.triggers.Trigger, ts: scala.collection.immutable.Seq[sta.model.triggers.Trigger]) =>
//              sta.model.triggers.Trigger.and(t +: ts)))
//      )"""
//    )
//  }
//}
