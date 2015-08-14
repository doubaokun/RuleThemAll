import Dependencies._
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

    addCompilerPlugin(paradise)
  )

  def androidSettings: Seq[Def.Setting[_]] = commonSettings ++ Seq(
    platformTarget in Android := versions.androidSDK,
    dexMaxHeap in Android := "2048m",
    transitiveAndroidLibs in Android := false,
    debugIncludesTests in Android := false
  ) ++ stdProguardSettings

  def appAndroidSettings: Seq[Def.Setting[_]] = androidBuild ++ androidSettings ++
    robolectricSettings ++ stdLibs ++ lintingSettings

  def libAndroidSettings: Seq[Def.Setting[_]] = androidBuildAar ++ androidSettings ++
    robolectricSettings ++ stdLibs ++ lintingSettings

  private def sources(in: File): Seq[File] = {
    if (in.isDirectory) in.listFiles(fileFilters.isScalaFile) ++
      in.listFiles(fileFilters.isDirectory).flatMap(sources)
    else Seq.empty
  }

  def benchmarkSettings: Seq[Def.Setting[_]] =
    androidBuild ++ androidSettings ++ Seq(
      libraryDependencies ++= benchmarks, //++ tests,

      fork in run := true,
      parallelExecution in test := false,

      apkbuildExcludes in Android ++= Seq(
        "META-INF/LICENSE.txt",
        "META-INF/NOTICE.txt"
      ),

      proguardOptions in Android ++= Seq(
        "-keep class sta.tests.benchmarks.**",
        "-keep class org.scalameter.**",
        "-keepattributes InnerClasses",
        "-dontwarn org.scalameter.utils.**",
        "-dontwarn org.apache.commons.math3.geometry.euclidean.**",
        "-dontwarn com.google.monitoring.runtime.instrumentation.**",
        "-dontwarn sun.misc.Unsafe",
        "-dontwarn com.google.common.collect.MinMaxPriorityQueue"
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
      /*,
      "-Ywarn-value-discard"
      */
    )
  ) ++ wartremoverSettings ++ Seq(
    wartremoverErrors ++= Seq(
      Wart.Any,
      Wart.Any2StringAdd,
      Wart.DefaultArguments,
      Wart.EitherProjectionPartial,
      Wart.JavaConversions,
      Wart.ListOps,
      Wart.MutableDataStructures,
      Wart.Nothing,
      Wart.Null,
      Wart.OptionPartial,
      Wart.Product,
      Wart.Return,
      Wart.Serializable,
      Wart.TryPartial,
      Wart.Var
      /*,
      Wart.AsInstanceOf,
      Wart.IsInstanceOf,
      Wart.NoNeedForMonad,
      Wart.NonUnitStatements,
      Wart.Throw
      */
    ),
    wartremoverExcluded ++= sources(target.value / "android-gen") ++
      sources(sourceDirectory.value / "test" / "scala")
  ) ++ ScalastylePlugin.projectSettings ++ Seq(
    ScalastylePlugin.scalastyleFailOnError := true
  )

  private def stdLibs: Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      cats,
      shapeless
    ) ++ tests.map(_ % "test")
  )

  private def robolectricSettings: Seq[Def.Setting[_]] = Seq(
    resolvers += "RoboTest releases" at "https://raw.github.com/zbsz/mvn-repo/master/releases/",
    libraryDependencies ++= robolectricTests.map(_ % "test"),
    fork in Test := true,
    javaOptions in Test ++= Seq("-XX:MaxPermSize=2048M", "-XX:+CMSClassUnloadingEnabled")
  )

  private def stdProguardSettings = Seq (
    proguardOptions in Android ++= Seq(
      "-keep public class * extends junit.framework.TestCase",
      "-keepclassmembers class * extends junit.framework.TestCase { *; }",
      "-keepattributes Signature",
      "-dontwarn org.typelevel.discipline.**",
      "-dontwarn spire.macros.**"
    ),

    proguardCache in Android ++= Seq(
      "algebra",
      "cats",
      "fastparse",
      "scala",
      "shapeless",
      "spire"
    )
  )
}
