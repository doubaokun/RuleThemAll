import Extras._
import sbt._

object Dependencies {
  object versions {
    val androidSDK = "android-19"
    val androidSupport = "19.0.0"
    val scala = "2.11.7"
    val paradise = "2.1.0-M5"

    object lib {
      val cats = "0.2.0"
      val fastparse = "0.2.1"
      val shapeless = "2.2.5"
      val spire = "0.10.1"
    }

    object tests {
      val scalacheck = "1.12.5"
      val scalameter = "0.7"
      val scalamock = "3.2.2"
      val scalatest = "2.2.5"
    }

    object ui {
      val macroid = "2.0.0-M4"
    }
  }

  val repositories = Seq(
    "Scalaz bintray" at "http://dl.bintray.com/scalaz/releases",
    "linter" at "http://hairyfotr.github.io/linteRepo/releases",
    Resolver.sonatypeRepo("snapshots")
  )

  val `android-support-v4` = "com.android.support" % "support-v4" % versions.androidSupport

  val paradise = "org.scalamacros" % "paradise" % versions.paradise cross CrossVersion.full

  val fastparse = "com.lihaoyi" %% "fastparse" % versions.lib.fastparse

  val reflect = "org.scala-lang" % "scala-reflect" % versions.scala

  val macrosExtra = "org.scalamacros" %% "resetallattrs" % "1.0.0"

  val cats = "org.spire-math" %% "cats"  % versions.lib.cats

  val shapeless = "com.chuusai" %% "shapeless" % versions.lib.shapeless

  val spire = "org.spire-math" %% "spire" % versions.lib.spire

  val enumeratum = "com.beachape" %% "enumeratum" % "1.2.3"

  val `scalameter-core` = "com.storm-enroute" %% "scalameter-core" % versions.tests.scalameter

  val scalameter = "com.storm-enroute" %% "scalameter" % versions.tests.scalameter

  val tests = Seq(
    "org.scalacheck" %% "scalacheck" % versions.tests.scalacheck,
    "org.scalamock" %% "scalamock-scalatest-support" % versions.tests.scalamock,
    "org.scalatest" %% "scalatest" % versions.tests.scalatest
  )

  val robolectricTests = Seq(
    "com.geteit" %% "robotest" % "0.12"
  )

  val macroid = Seq(
    "org.macroid" %% "macroid" % versions.ui.macroid,
    "org.macroid" %% "macroid-viewable" % versions.ui.macroid
  )

  val `android-support` = "com.android.support" % "support-v4" % "21.0.0"

  val rootTools = GithubRelease("Stericson/RootTools", "4.2", "RootTools.jar")
}
