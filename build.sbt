import Dependencies._
import Extras._
import Settings._


name := "Rule 'Em All"

organization := "rta"

commonSettings

lazy val utils = project.in(file("utils")).settings(
  libraryDependencies ++= Seq(
    reflect
  )
).settings(androidBuildAar ++ commonSettings ++ androidSettings ++ externalJarsSettings: _*)
  .dependsOnExternal(rootTools)

lazy val core = project.in(file("core")).settings(
  libraryDependencies ++= Seq(
    enumeratum,
    fastparse,
    macrosExtra,
    reflect,
    spire
  )
).settings(libAndroidSettings: _*).dependsOnLocal(utils).excludeFromLinting(
    _ / "rta" / "common" / "usesMacros",
    _ / "rta" / "model" / "triggers" / "functions" / "ModelFunction",
    _ / "rta" / "parser" / *,
    _ / "rta" / "service" / "serviceMacros"
  )

lazy val app = project.in(file("app")).settings(
  libraryDependencies ++= Seq(
    `android-support-v4`
  )
).settings(libAndroidSettings: _*).dependsOnLocal(core, utils).excludeFromLinting(
    _ / "rta" / "parser" / **,
    _ / "rta" / "service" / "PluginHandler",
    _ / "rta" / "service" / "RulesService"
  )

lazy val tests = project.in(file("tests")).settings(testsSettings: _*)
  .dependsOnLocal(app, core, utils)

lazy val gui = project.in(file("gui")).settings(appAndroidSettings)
  .dependsOnLocal(app, core, utils)

lazy val root = project.in(file(".")).aggregate(utils, core, app, tests, gui).settings(Seq(
  parallelExecution in Android := false
))
