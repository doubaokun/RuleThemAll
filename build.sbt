
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
    parboiled,
    reflect,
    spire
  ) ++ shapeless,
  buildConfigGenerator in Android := Seq.empty
).settings(libAndroidSettings: _*).dependsOnLocal(utils).excludeFromLinting(
    _ / "sta" / "common" / "Common.scala",
    _ / "sta" / "model" / "definition.scala",
    _ / "sta" / "parser" / "macros.scala",
    _ / "sta" / "services" / "macros.scala"
  )

lazy val core = project.in(file("core")).settings(
  libraryDependencies ++= Seq(
    `android-support-v4`,
    parboiled,
    scodec.core
  ) ++ shapeless
).settings(libAndroidSettings: _*).dependsOnLocal(`core-common`, utils).excludeFromLinting(
    _ / "sta" / "parser" / "**"
  )

lazy val tests = project.in(file("tests")).settings(benchmarkSettings: _*).dependsOnLocal(core, `core-common`, utils)

lazy val root = project.in(file(".")).aggregate(utils, `core-common`, core, tests)
