import sbt._
import android.Dependencies._

object Dependencies {
  object versions {
    val app = "0.1"
    val androidSDK = "android-20"
    val androidSupport = "20.0.0"
    val scala = "2.11.6"
    val paradise = "2.1.0-M5"

    object lib {
      val parboiled = "2.1.0"
      val scalaz = "7.1.1"
      val scodec = "1.7.0"
      val shapeless = "2.1.0"
      val spire = "0.9.1"
    }

    object tests {
      val scalacheck = "1.12.2"
      val scalamock = "3.2.1"
      val scalatest = "2.2.4"
    }
  }

  val repositories = Seq(
    "Scalaz bintray" at "http://dl.bintray.com/scalaz/releases",
    "linter" at "http://hairyfotr.github.io/linteRepo/releases",
    Resolver.sonatypeRepo("snapshots")
  )

  val `android-support-v4` = "com.android.support" % "support-v4" % versions.androidSupport

  val paradise = "org.scalamacros" % "paradise" % versions.paradise cross CrossVersion.full

  val parboiled = "org.parboiled" %% "parboiled" % versions.lib.parboiled

  val reflect = "org.scala-lang" % "scala-reflect" % versions.scala

  val rapture = Seq(
    "com.propensive" %% "rapture-json" % "1.0.6",
    "com.propensive" %% "rapture-json-jawn" % "1.0.6",
    "com.propensive" %% "rapture-uri" % "1.0.0",
    "com.propensive" %% "rapture-io" % "0.10.0"
  )

  object scalaz {
    val core = "org.scalaz" %% "scalaz-core" % versions.lib.scalaz

    val concurrent = "org.scalaz" %% "scalaz-concurrent" % versions.lib.scalaz

//    val stream = "org.scalaz.stream" %% "scalaz-stream" % "0.6a"
  }

  object scodec {
    val core = "org.scodec" %% "scodec-core" % versions.lib.scodec

//    val stream = "org.scodec" %% "scodec-stream" % "0.4.0"
  }

  val shapeless = Seq(
    "com.chuusai" %% "shapeless" % versions.lib.shapeless/*,
    "org.typelevel" %% "shapeless-spire" % "0.3",
    "org.typelevel" %% "shapeless-scalaz" % "0.3"*/
  )

  val spire = "org.spire-math" %% "spire" % versions.lib.spire

  val benchmarks = Seq(
    aar("com.artfulbits" % "meter" % "1.0.1.93")
      from "http://dl.bintray.com/kucherenko-alex/android/com/artfulbits/library/1.0.1.93/library-1.0.1.93.aar"
  )

  val tests = Seq(
    "org.scalacheck" %% "scalacheck" % versions.tests.scalacheck,
    "org.scalamock" %% "scalamock-scalatest-support" % versions.tests.scalamock,
    "org.scalatest" %% "scalatest" % versions.tests.scalatest
  )

  val robolectricTests = Seq(
    "org.robolectric" % "android-all" % "5.0.0_r2-robolectric-0",
    "com.geteit" %% "robotest" % "0.7",
    "junit" % "junit" % "4.8.2"
  )
}
