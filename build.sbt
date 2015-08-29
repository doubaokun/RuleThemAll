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

lazy val `core-common` = project.in(file("core/common")).settings(
  libraryDependencies ++= Seq(
    enumeratum,
    macrosExtra,
    reflect,
    spire
  )
).settings(libAndroidSettings: _*).dependsOnLocal(utils).excludeFromLinting(
    _ / "sta" / "common" / "Requirement",
    _ / "sta" / "common" / "usesMacros",
    _ / "sta" / "model" / "Rule",
    _ / "sta" / "model" / "triggers" / "trigger",
    _ / "sta" / "model" / "triggers" / "functions" / "ModelFunction",
    _ / "sta" / "services" / "serviceMacros"
  )

lazy val core = project.in(file("core")).settings(
  libraryDependencies ++= Seq(
    `android-support-v4`,
    fastparse
  )
).settings(libAndroidSettings: _*).dependsOnLocal(`core-common`, utils).excludeFromLinting(
    _ / "sta" / "model" / "actions" / **,
    _ / "sta" / "model" / "triggers" / **,
    _ / "sta" / "parser" / **,
    _ / "sta" / "storage" / "PlaintextStorage",
    _ / "sta" / "services" / "package",
    _ / "sta" / "services" / "service"
  )

lazy val tests = project.in(file("tests")).settings(benchmarkSettings: _*)
  .dependsOnLocal(core, `core-common`, utils)


lazy val root = project.in(file(".")).aggregate(utils, `core-common`, core, tests).settings(Seq(
  parallelExecution in Android := false
))
