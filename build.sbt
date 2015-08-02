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
).settings(libAndroidSettings ++ wartremover.wartremoverSettings: _*)

lazy val `core-common` = project.in(file("core/common")).settings(
  libraryDependencies ++= Seq(
    enumeratum,
    fastparse,
    macrosExtra,
    reflect,
    spire
  ) ++ shapeless ++ records,
  buildConfigGenerator in Android := Seq.empty
).settings(libAndroidSettings: _*).dependsOnLocal(utils).excludeFromLinting(
    _ / "sta" / "common" / "Common",
    _ / "sta" / "model" / "definition",
    _ / "sta" / "model" / "triggers" / "functions" / "ModelFunction",
    _ / "sta" / "services" / "macros"
  )

lazy val core = project.in(file("core")).settings(
  libraryDependencies ++= Seq(
    `android-support-v4`,
    fastparse,
    scodec.core
  ) ++ shapeless ++ records
).settings(libAndroidSettings: _*).dependsOnLocal(`core-common`, utils).excludeFromLinting(
    _ / "sta" / "parser" / **,
    _ / "sta" / "model" / "system" / **
  )

lazy val tests = project.in(file("tests")).settings(benchmarkSettings: _*)
  .dependsOnLocal(core, `core-common`, utils)

lazy val root = project.in(file(".")).aggregate(utils, `core-common`, core, tests)
