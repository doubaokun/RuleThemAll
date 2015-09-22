package sta.storage

import android.content.Context
import java.io._
import kj.android.common.{AppInfo, Notify, Toast}
import scala.collection.mutable
import sta.model.Rule
import sta.parser.RulesParser

class PlaintextStorage(implicit val ctx: Context, val info: AppInfo) extends RulesStorage {
  private[this] val rulesDir: File = ctx.getDir("rules", Context.MODE_PRIVATE)

  private[this] val rawRules = {
    val map = mutable.Map.empty[String, Rule]
    val files = rulesDir.listFiles(new FilenameFilter {
      def accept(dir: File, filename: String): Boolean = filename.endsWith(".rule")
    })
    for (file <- files) {
      val rule = RulesParser.parseSingle(io.Source.fromFile(file).mkString)
      map += (rule.name -> rule)
    }
    Toast(s"${files.length} rules loaded")
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

  private def parse(from: File): Set[Rule] = {
    val input = io.Source.fromFile(from).mkString
    val rules = RulesParser.tracedParse(input).fold(
      err => {
        log.error(s"Failed to parse rules from ${from.getPath}", err)
        Notify(s"Failed to parse rules from ${from.getPath}", Some(logTag.tag)) // TODO add notification action
        Set.empty[Rule]
      },
      res => {
        var from = 0
        val result: Set[Rule] = res.map { case (rule, trace) =>
          val fos = new FileOutputStream(new File(rulesDir, s"${rule.name}.rule"))
          try {
            val s = input.substring(from, trace)
            fos.write(s.trim.getBytes)
          } finally {
            fos.close()
          }
          from = trace
          rule
        }(collection.breakOut)
        result
      }
    )
    rules
  }

  def allRules: Iterator[Rule] = synchronized {
    rawRules.valuesIterator
  }

  def get(name: String) = synchronized {
    rawRules.get(name)
  }

  def register(from: File): RegistrationInfo = synchronized {
    val rules = parse(from)
    val added = Set.newBuilder[Int]
    val removed = Set.newBuilder[Int]

    // remove old uses
    for (
      rule <- rules;
      oldRule <- rawRules.get(rule.name);
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
    rawRules ++= rules.map(r => r.name -> r)
    val currSize = rawRules.size
    Toast(txt = s"${currSize - prevSize} rules inserted, " +
      s"${prevSize + rules.size - currSize} rules updated", length = Toast.Long)
    RegistrationInfo(
      addedRequirements = added.result(),
      removedRequirements = removed.result() -- added.result(),
      addedRules = rules
    )
  }

  def unregister(names: String*): Set[Int] = synchronized {
    val removed = Set.newBuilder[Int]
    names.foreach { name =>
      val file = new File(rulesDir, s"$name.rule")
      val rule = RulesParser.parseSingle(io.Source.fromFile(file).mkString)

      rawRules -= rule.name
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

    Toast(txt = s"${names.length} rules removed", length = Toast.Long)
    removed.result()
  }
}