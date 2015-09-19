import Dependencies._
import Settings._


name := "Script 'Em All"

organization := "sta"

version := versions.app

commonSettings

lazy val utils = project.in(file("utils")).settings(
  libraryDependencies ++= Seq(
    reflect
  )
).settings(androidBuildAar ++ commonSettings ++ androidSettings: _*)

lazy val core = project.in(file("core")).settings(
  libraryDependencies ++= Seq(
    enumeratum,
    fastparse,
    macrosExtra,
    reflect,
    spire
  )
).settings(libAndroidSettings: _*).dependsOnLocal(utils).excludeFromLinting(
    _ / "sta" / "common" / "Requirement",
    _ / "sta" / "common" / "usesMacros",
    _ / "sta" / "model" / "Rule",
    _ / "sta" / "model" / "model",
    _ / "sta" / "model" / "triggers" / "Trigger",
    _ / "sta" / "model" / "triggers" / "functions" / "ModelFunction",
    _ / "sta" / "parser" / *,
    _ / "sta" / "services" / "RulesExecutor",
    _ / "sta" / "services" / "serviceMacros"
  )

lazy val plugin = project.in(file("plugin")).settings(libAndroidSettings: _*)
  .dependsOnLocal(core, utils).excludeFromLinting(
    _ / "sta" / "plugin" / "Plugin"
  )

lazy val app = project.in(file("app")).settings(
  libraryDependencies ++= Seq(
    `android-support-v4`
  )
).settings(libAndroidSettings: _*).dependsOnLocal(core, plugin, utils).excludeFromLinting(
    _ / "sta" / "model" / "actions" / **,
    _ / "sta" / "model" / "triggers" / **,
    _ / "sta" / "parser" / **,
    _ / "sta" / "services" / "PluginHandler",
    _ / "sta" / "services" / "STAService",
    _ / "sta" / "services" / "TimerMap",
    _ / "sta" / "services" / "package",
    _ / "sta" / "storage" / "PlaintextStorage"
  )

lazy val tests = project.in(file("tests")).settings(testsSettings: _*)
  .dependsOnLocal(app, core, utils)

lazy val root = project.in(file(".")).aggregate(utils, core, app, tests).settings(Seq(
  parallelExecution in Android := false
))
