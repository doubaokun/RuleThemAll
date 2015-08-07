package sta.storage

import android.content.Context
import java.io._
import scala.collection.mutable
import sta.model.Rule
import sta.parser.RulesParser

class FileRulesStorage(val ctx: Context) extends RulesStorage {
  private lazy val rulesDir: File = ctx.getDir("rules", Context.MODE_PRIVATE)

  private lazy val rawRules = mutable.Set[Rule](rulesDir.listFiles(new FilenameFilter {
    def accept(dir: File, filename: String): Boolean = filename.endsWith(".rule")
  }).map(deserialize): _*)

  private lazy val rawUses = {
    val map = mutable.Map.empty[String, Int]
    for (
      rule <- rawRules;
      use <- rule.uses
    ) {
      map += (use -> (map.getOrElse(use, 0) + 1))
    }
    map
  }

  private def parse(from: File): Seq[Rule] = {
    val input = io.Source.fromFile(from).mkString
    RulesParser.parse(input).fold(
      err => {
        log.error(s"Failed to parse rules from ${from.getPath}", err)
        Seq.empty[Rule]
      },
      identity
    )
  }

  private def deserialize(file: File): Rule = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    try {
      ois.readObject().asInstanceOf[Rule]
    } finally {
      ois.close()
    }
  }

  private def serialize(rule: Rule, dir: File): Unit = {
    val file = new File(dir, s"${rule.name}.rule")
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    try {
      oos.writeObject(rule)
    } finally {
      oos.close()
    }
  }

  def rules: Iterator[Rule] = rawRules.iterator

  def register(from: File): Set[String] = {
    val rules = parse(from)

    rules.foreach(serialize(_, rulesDir))
    rawRules ++= rules

    val added = Set.newBuilder[String]
    for (
      rule <- rules;
      use <- rule.uses
    ) {
      rawUses.get(use).fold[Unit] {
        added += use
        rawUses += (use -> 1)
      }(v => rawUses += (use -> (v + 1)))
    }
    added.result()
  }

  def unregister(names: String*): Set[String] = {
    val removed = Set.newBuilder[String]
    names.foreach { name =>
      val file = new File(rulesDir, s"$name.rule")
      val rule = deserialize(file)

      rawRules -= rule
      file.delete()

      rule.uses.foreach { use =>
        rawUses.get(use).fold[Unit]() {
          case 1 =>
            removed += use
            rawUses -= use
          case v =>
            rawUses += (use -> (v - 1))
        }
      }
    }
    removed.result()
  }
}
