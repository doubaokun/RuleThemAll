package rta.storage

import android.content.Context
import fastparse.core.{Result, Parser}
import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock
import rta.common.{Utils, AppInfo}
import rta.logging.Logging
import rta.model.Rule
import rta.parser.RulesParser

@SuppressWarnings(Array(
  "org.brianmckenna.wartremover.warts.Null",
  "org.brianmckenna.wartremover.warts.Var"
))
abstract class RulesStorage(implicit ctx: Context, info: AppInfo) extends Logging {
  type T

  def parserCacheRegen: () => RulesParser.Cached[T]

  private[this] val lock = new ReentrantReadWriteLock()
  private[this] var rawParserCache: RulesParser.Cached[T] = null.asInstanceOf[RulesParser.Cached[T]]
  
  /** Unregisters rules.
    *
    * @param names rules to unregister
    * @return set of removed intent hash codes
    */
  def unregister(names: String*): Set[Int]

  /** Registers rules.
    *
    * @param from location of rule files
    * @return tuple (set of added intent hash codes, set of removed intent hash codes, set of added rule names)
    */
  def register(from: File*): RegistrationInfo

  def allRules: Iterator[Rule]

  def rules: Iterator[Rule] = allRules.filter(_.branches.nonEmpty)

  def get(name: String): Option[Rule]

  def startupRules: Iterator[Rule] = allRules.filter(_.branches.isEmpty)
  
  def cacheParser(): Unit = Utils.inLock(lock.writeLock()) {
    if (rawParserCache eq null) rawParserCache = parserCacheRegen()
  }
  
  def invalidateParser(): Unit = Utils.withGC(Utils.inLock(lock.writeLock()) {
    rawParserCache = null.asInstanceOf[RulesParser.Cached[T]]
  })

  def recacheParser(): Unit = Utils.inLock(lock.writeLock()) {
    if (rawParserCache ne null) rawParserCache = parserCacheRegen()
  }
  
  def parser: RulesParser.Cached[T] = Utils.inLock(lock.readLock()) {
    if (rawParserCache ne null) rawParserCache else parserCacheRegen()
  }
}
