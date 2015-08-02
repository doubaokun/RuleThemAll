import sbt._


object Dependencies {
  object versions {
    val app = "0.1"
    val androidSDK = "android-20"
    val androidSupport = "20.0.0"
    val scala = "2.11.7"
    val paradise = "2.1.0-M5"

    object lib {
      val fastparse = "0.2.1"
      val scalaz = "7.1.1"
      val scodec = "1.7.0"
      val shapeless = "2.2.0"
      val spire = "0.9.1"
    }

    object tests {
      val scalacheck = "1.12.4"
      val scalatest = "2.2.5"
      val scalameter = "0.7-SNAPSHOT"
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

  val records = Seq(
    "ch.epfl.lamp" %% "scala-records" % "0.4-SNAPSHOT",
    "org.cvogt" %% "compossible" % "0.2-SNAPSHOT"
  )

  val spire = "org.spire-math" %% "spire" % versions.lib.spire

  val enumeratum = "com.beachape" %% "enumeratum" % "1.2.3"

  val benchmarks = Seq(
    "com.storm-enroute" %% "scalameter-core" % versions.tests.scalameter
  )

  val tests = Seq(
    "org.scalacheck" %% "scalacheck" % versions.tests.scalacheck,
    "org.scalatest" %% "scalatest" % versions.tests.scalatest
  )

  val robolectricTests = Seq(
    "com.geteit" %% "robotest" % "0.12"
  )
}
