package rta.logging

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Logger {
  def debug(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.debugMsg

  def debug(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.debugMsgCause

  def error(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.errorMsg

  def error(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.errorMsgCause

  def info(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.infoMsg

  def info(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.infoMsgCause

  def verbose(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.verboseMsg

  def verbose(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.verboseMsgCause

  def warn(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.warnMsg

  def warn(cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.warnCause

  def warn(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.warnMsgCause

  def wtf(msg: String)(implicit lt: LogTag): Unit = macro LoggerMacros.wtfMsg

  def wtf(cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.wtfCause

  def wtf(msg: String, cause: Throwable)(implicit lt: LogTag): Unit = macro LoggerMacros.wtfMsgCause
}

private class LoggerMacros(val c: blackbox.Context) {

  import c.universe._

  private def log = typeOf[android.util.Log].companion

  def debugMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.DEBUG)) $log.d($lt.tag, $msg)"

  def debugMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.DEBUG)) $log.d($lt.tag, $msg, $cause)"

  def errorMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.ERROR)) $log.e($lt.tag, $msg)"

  def errorMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.ERROR)) $log.e($lt.tag, $msg, $cause)"

  def infoMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.INFO)) $log.i($lt.tag, $msg)"

  def infoMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.INFO)) $log.i($lt.tag, $msg, $cause)"

  def verboseMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.VERBOSE)) $log.v($lt.tag, $msg)"

  def verboseMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.VERBOSE)) $log.v($lt.tag, $msg, $cause)"

  def warnMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.WARN)) $log.w($lt.tag, $msg)"

  def warnCause(cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.WARN)) $log.w($lt.tag, $cause)"

  def warnMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.WARN)) $log.w($lt.tag, $msg, $cause)"

  def wtfMsg(msg: Expr[String])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.ASSERT)) $log.wtf($lt.tag, $msg)"

  def wtfCause(cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.ASSERT)) $log.wtf($lt.tag, $cause)"

  def wtfMsgCause(msg: Expr[String], cause: Expr[Throwable])(lt: Expr[LogTag]) =
    q"if ($log.isLoggable($lt.tag, $log.ASSERT)) $log.wtf($lt.tag, $msg, $cause)"
}
