package sta.storage

import android.content.Context
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
    val map = mutable.Map.empty[Int, Int]
    for (
      rule <- rawRules.valuesIterator;
      requirement <- rule.requires
    ) {
      val hash = requirement.hashCode()
      map += (hash -> (map.getOrElse(hash, 0) + 1))
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

  def register(from: File): (Set[Int], Set[Int]) = synchronized {
    val rules = parse(from)
    val added = Set.newBuilder[Int]
    val removed = Set.newBuilder[Int]

    // remove old uses
    for (
      rule <- rules;
      oldRule <- rawRules.get(rule.hashCode());
      requirement <- oldRule.requires;
      count <- rawUses.get(requirement.hashCode())
    ) {
      count match {
        case 1 =>
          removed += requirement.hashCode()
          rawUses -= requirement.hashCode()
        case _ =>
          rawUses += (requirement.hashCode() -> (count - 1))
      }
    }

    // add new uses
    for (
      rule <- rules;
      requirement <- rule.requires
    ) {
      rawUses.get(requirement.hashCode()).fold[Unit] {
        added += requirement.hashCode()
        rawUses += (requirement.hashCode() -> 1)
      }(count => rawUses += (requirement.hashCode() -> (count + 1)))
    }

    val prevSize = rawRules.size
    rawRules ++= rules.map(r => r.hashCode() -> r)
    val currSize = rawRules.size
    Notify(s"${currSize - prevSize} rules inserted, " +
      s"${prevSize + rules.length - currSize} rules updated", Some(logTag.tag))
    (added.result(), removed.result() -- added.result())
  }

  def unregister(names: String*): Set[Int] = synchronized {
    val removed = Set.newBuilder[Int]
    names.foreach { name =>
      val file = new File(rulesDir, s"$name.rule")
      val rule = RulesParser.parseSingle(io.Source.fromFile(file).mkString)

      rawRules -= rule.hashCode()
      file.delete()
      for (
        requirement <- rule.requires;
        count <- rawUses.get(requirement.hashCode())
      ) {
        count match {
          case 1 =>
            removed += requirement.hashCode()
            rawUses -= requirement.hashCode()
          case _ =>
            rawUses += (requirement.hashCode() -> (count - 1))
        }
      }
    }

    Notify(s"${names.length} rules removed", Some(logTag.tag))
    removed.result()
  }
}
