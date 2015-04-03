package sta.store

import java.io.File

import android.content.Context
import android.net.Uri

import kj.android.logging.Logging
import sta.model.Definition

//case class ScriptPath(path: Uri) extends AnyVal

abstract class DefinitionsStore extends Logging {
  private def scriptDir(implicit ctx: Context): File = ctx.getDir("scripts", Context.MODE_PRIVATE)

  protected def deserialize(from: File): Seq[Definition]

  //  def unregisterDefinitions(path: ScriptPath): Set[String] = ???
  //
  //  def registerDefinitions(path: ScriptPath): Set[String] = ???

  final def definitions(implicit ctx: Context): Seq[Definition] = {
    scriptDir.listFiles().flatMap(deserialize)
  }
}
