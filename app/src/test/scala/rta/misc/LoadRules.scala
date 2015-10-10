package rta.misc

import rta.parser.RulesParser

trait LoadRules {
  def loadResource(file: String) = io.Source.fromInputStream(getClass.getResourceAsStream(s"/$file")).mkString
  
  def loadRulesFromResource(file: String) = RulesParser.Multi.parse(loadResource(file)).get.value.iterator
}
