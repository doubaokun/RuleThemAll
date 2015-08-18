package sta.storage

import android.content.Context
import android.widget.Toast
import java.io._
import kj.android.common.{AppInfo, Notify}
import scala.collection.mutable
import sta.model.Rule
import sta.parser.RulesParser

class PlaintextStorage(implicit val ctx: Context, val info: AppInfo) extends RulesStorage {
  private[this] val rulesDir: File = ctx.getDir("rules", Context.MODE_PRIVATE)

  private[this] val rawRules = {
    val map = mutable.Map.empty[Int, Rule]
    val files = rulesDir.listFiles(new FilenameFilter {
      def accept(dir: File, filename: String): Boolean = filename.endsWith(".rule")
    })
    for (file <- files) {
      val rule = RulesParser.parseSingle(io.Source.fromFile(file).mkString)
      map += (rule.hashCode() -> rule)
    }
    Notify(s"${files.length} rules loaded", Some(logTag.tag))
    map
  }

  private[this] val rawUses = {
    val map = mutable.Map.empty[String, Int]
    for (
      rule <- rawRules.valuesIterator;
      use <- rule.uses
    ) {
      map += (use -> (map.getOrElse(use, 0) + 1))
    }
    map
  }

  private def parse(from: File): Seq[Rule] = {
    val input = io.Source.fromFile(from).mkString
    val rules = RulesParser.tracedParse(input).fold(
      err => {
        log.error(s"Failed to parse rules from ${from.getPath}", err)
        Seq.empty[Rule]
      },
      res => {
        var from = 0
        res.map { case (rule, trace) =>
          val fos = new FileOutputStream(new File(rulesDir, s"${rule.name}.rule"))
          try {
            val s = input.substring(from, trace)
            fos.write(s.trim.getBytes)
          } finally {
            fos.close()
          }
          from = trace
          rule
        }
      }
    )
    rules
  }

  def allRules: Iterator[Rule] = synchronized {
    rawRules.valuesIterator
  }

  def register(from: File): (Set[String], Set[String]) = synchronized {
    val rules = parse(from)
    val added = Set.newBuilder[String]
    val removed = Set.newBuilder[String]

    // remove old uses
    for (
      rule <- rules;
      oldRule <- rawRules.get(rule.hashCode());
      use <- oldRule.uses;
      count <- rawUses.get(use)
    ) {
      count match {
        case 1 =>
          removed += use
          rawUses -= use
        case _ =>
          rawUses += (use -> (count - 1))
      }
    }
    // add new uses
    for (
      rule <- rules;
      use <- rule.uses
    ) {
      rawUses.get(use).fold[Unit] {
        added += use
        rawUses += (use -> 1)
      }(count => rawUses += (use -> (count + 1)))
    }

    val prevSize = rawRules.size
    rawRules ++= rules.map(r => r.hashCode() -> r)
    val currSize = rawRules.size
    Notify(s"${currSize - prevSize} rules inserted, " +
      s"${prevSize + rules.length - currSize} rules updated", Some(logTag.tag))
    (added.result(), removed.result() -- added.result())
  }

  def unregister(names: String*): Set[String] = synchronized {
    val removed = Set.newBuilder[String]
    names.foreach { name =>
      val file = new File(rulesDir, s"$name.rule")
      val rule = RulesParser.parseSingle(io.Source.fromFile(file).mkString)

      rawRules -= rule.hashCode()
      file.delete()
      for (
        use <- rule.uses;
        count <- rawUses.get(use)
      ) {
        count match {
          case 1 =>
            removed += use
            rawUses -= use
          case _ =>
            rawUses += (use -> (count - 1))
        }
      }
    }

    Notify(s"${names.length} rules removed", Some(logTag.tag))
    removed.result()
  }
}
