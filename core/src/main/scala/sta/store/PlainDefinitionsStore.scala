package sta.store

import java.io.File
import sta.model.Definition
import sta.parser.DSLParser

// stores definitions in unmodified form
// TODO: what to do on error ?
object PlainDefinitionsStore extends DefinitionsStore {
  protected def deserialize(from: File): Seq[Definition] = {
    val input = io.Source.fromFile(from).mkString
    DSLParser.parse(input).fold(
      err => {
        log.error(s"Failed to deserialize definitions from ${from.getName}", err)
        Seq.empty[Definition]
      },
      identity
    )

  }
}
