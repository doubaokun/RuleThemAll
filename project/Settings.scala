import Dependencies._
import Extras._
import android.Keys._
import android.Plugin._
import java.io.FilenameFilter
import org.scalastyle.sbt.ScalastylePlugin
import sbt.Keys._
import sbt._
import wartremover._

object Settings {
  sealed trait Special
  case object * extends Special
  case object ** extends Special

  implicit def liftToSeq[T](obj: T): Seq[T] = Seq(obj)

  implicit class RichFile(val file: File) {
    def /(special: Special): Seq[File] = special match {
      case * => file.listFiles(fileFilters.isScalaFile)
      case ** => sources(file)
    }
  }

  private object fileFilters {
    val isScalaFile = new FilenameFilter {
      def accept(dir: File, filename: String): Boolean =
        filename.endsWith(".scala") && new File(dir, filename).isFile
    }

    val isDirectory = new FilenameFilter {
      def accept(dir: File, filename: String): Boolean = new File(dir, filename).isDirectory
    }
  }

  implicit class RichProject(val project: Project) {
    def dependsOnLocal(projects: Project*): Project = {
      project.settings(
        localProjects in Android ++= projects.map(p => LibraryProject(p.base)): _*
      ).dependsOn(projects.map(new ClasspathDependency(_, Some("compile->compile;test->test"))): _*)
    }

    def dependsOnExternal(projects: ExternalJar*): Project = {
      project.settings(externalJars in Compile ++= projects)
    }

    def excludeFromLinting(files: (sbt.File => Seq[sbt.File])*): Project = {
      project.settings(
        wartremoverExcluded ++= (for {
          f <- files
          file <- f(sourceDirectory.value / "main" / "scala")
        } yield {
            if (file.getName.matches(".*\\..*")) file
            else new File(file.getParentFile, s"${file.getName}.scala")
          })
      )
    }
  }

  def commonSettings: Seq[Def.Setting[_]] = Seq(
    scalaVersion := versions.scala,
    incOptions := incOptions.value.withNameHashing(nameHashing = true)
      .withRecompileOnMacroDef(recompileOnMacroDef = true),

    javaOptions += "-Xmx2G",

    resolvers ++= repositories,

    addCompilerPlugin(paradise),

    libraryDependencies ++= tests.map(_ % "test")
  )

  def androidSettings: Seq[Def.Setting[_]] = commonSettings ++ Seq(
    platformTarget in Android := versions.androidSDK,
    dexMaxHeap in Android := "2048m",
    transitiveAndroidLibs in Android := false,
    debugIncludesTests in Android := false
  ) ++ stdProguardSettings

  def appAndroidSettings: Seq[Def.Setting[_]] = androidBuild ++ androidSettings ++
    robolectricSettings ++ lintingSettings ++ uiSettings ++ Seq(
    wartremoverErrors := Warts.allBut(Wart.MutableDataStructures, Wart.Var,
      Wart.DefaultArguments, Wart.ExplicitImplicitTypes, Wart.NonUnitStatements, Wart.Throw,
      Wart.Any, Wart.Nothing, Wart.Null, Wart.Product, Wart.NoNeedForMonad/** TODO report bug */)
  )

  def libAndroidSettings: Seq[Def.Setting[_]] = androidBuildAar ++ androidSettings ++
    robolectricSettings ++ stdLibs ++ lintingSettings

  private def sources(in: File): Seq[File] = {
    if (in.isDirectory) in.listFiles(fileFilters.isScalaFile) ++
      in.listFiles(fileFilters.isDirectory).flatMap(sources)
    else Seq.empty
  }

  def testsSettings: Seq[Def.Setting[_]] =
    androidBuild ++ androidSettings ++ Seq(
      libraryDependencies ++= benchmarks ++ tests,

      debugIncludesTests in Android := true,

      fork in run := true,
      parallelExecution in test := false,

      packagingOptions in Android := PackagingOptions(
        excludes = Seq(
          "META-INF/LICENSE.txt",
          "META-INF/NOTICE.txt"
        )
      ),

      proguardOptions in Android ++= Seq(
        "-keep public class * extends junit.framework.TestCase",
        "-keepclassmembers class * extends junit.framework.TestCase { *; }",
        "-keep class org.scalameter.**",
        "-keep class sta.tests.benchmarks.**",

        "-keepattributes InnerClasses",

        "-dontwarn com.google.common.collect.MinMaxPriorityQueue",
        "-dontwarn com.google.monitoring.runtime.instrumentation.**",
        "-dontwarn org.apache.commons.math3.geometry.euclidean.**",
        "-dontwarn org.scalacheck.**", // TODO
        "-dontwarn org.scalameter.utils.**",
        "-dontwarn org.scalatest.junit.**",
        "-dontwarn org.scalatest.mock.**",
        "-dontwarn org.scalatest.selenium.**",
        "-dontwarn org.scalatest.tools.**",
        "-dontwarn org.testng.**",
        "-dontwarn sun.misc.Unsafe"
      ),

      proguardCache in Android ++= Seq(
        "org.scalacheck",
        "org.scalactic",
        "org.scalameter",
        "org.scalamock",
        "org.scalatest"
      )
    )

  private def uiSettings: Seq[Def.Setting[_]] = Seq(
    resolvers += "jcenter" at "http://jcenter.bintray.com",
    libraryDependencies ++= Seq(
      `android-support`
    ) ++ macroid.map(aar),

    proguardCache in Android ++= Seq(
      "android.support",
      "macroid"
    )
  )

  private def lintingSettings: Seq[Def.Setting[_]] = Seq(
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Xfuture"
    )
  ) ++ wartremoverSettings ++ Seq(
    wartremoverErrors ++= Warts.allBut(Wart.NonUnitStatements, Wart.ExplicitImplicitTypes,
      Wart.Any, Wart.Nothing, Wart.Product, Wart.Serializable, Wart.IsInstanceOf, Wart.AsInstanceOf,
      Wart.Throw, Wart.NoNeedForMonad),
    wartremoverExcluded ++= sources(target.value / "android") ++
      sources(sourceDirectory.value / "test" / "scala")
  ) ++ ScalastylePlugin.projectSettings ++ Seq(
    ScalastylePlugin.scalastyleFailOnError := true
  )

  private def stdLibs: Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      cats,
      shapeless
    )
  )

  private def robolectricSettings: Seq[Def.Setting[_]] = Seq(
    resolvers += "RoboTest releases" at "https://raw.github.com/zbsz/mvn-repo/master/releases/",
    libraryDependencies ++= robolectricTests.map(_ % "test"),
    fork in Test := true,
    javaOptions in Test ++= Seq("-XX:MaxPermSize=2048M", "-XX:+CMSClassUnloadingEnabled")
  )

  private def stdProguardSettings = Seq(
    proguardOptions in Android ++= Seq(
      "-keepattributes Signature",
      "-dontwarn org.typelevel.discipline.**",
      "-dontwarn spire.macros.**"
    ),

    proguardCache in Android ++= Seq(
      "algebra",
      "cats",
      "com.stericson",
      "fastparse",
      "org.brianmckenna.wartremover",
      "org.scalacheck",
      "shapeless",
      "spire"
    )
  )
}
