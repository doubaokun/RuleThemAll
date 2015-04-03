import java.io.FilenameFilter

import Dependencies._
import android.Keys._
import android.Plugin._
import com.typesafe.sbt.SbtScalariform._
import org.scalastyle.{sbt => scalastyle}
import sbt.Keys._
import sbt._
import wartremover._

import scalariform.formatter.preferences._

object Settings {
  private object fileFilters {
    val isScalaFile = new FilenameFilter {
      def accept(dir: File, filename: String): Boolean = filename.endsWith(".scala") && new File(dir, filename).isFile
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

    def excludeFromLinting(files: (sbt.File => sbt.File)*): Project = {
      project.settings(
        wartremoverExcluded ++= files.flatMap { f =>
          val file = f(sourceDirectory.value / "main" / "scala")
          file.getName match {
            case "*" => file.getParentFile.listFiles(fileFilters.isScalaFile)
            case "**" => excludeSources(file.getParentFile)
            case _ => Seq(file)
          }
        }
      )
    }
  }

  def commonSettings: Seq[Def.Setting[_]] = Seq(
    scalaVersion := versions.scala,
    incOptions := incOptions.value.withNameHashing(nameHashing = true).withRecompileOnMacroDef(recompileOnMacroDef = true),

    javaOptions += "-Xmx2G",

    resolvers ++= repositories,

    addCompilerPlugin(paradise)
  ) ++ scalariformSettings ++ Seq(
    ScalariformKeys.preferences := ScalariformKeys.preferences.value
      .setPreference(AlignParameters, true)
      .setPreference(AlignSingleLineCaseStatements, true)
      .setPreference(DoubleIndentClassDeclaration, true)
      .setPreference(PreserveDanglingCloseParenthesis, true)
      .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
      .setPreference(RewriteArrowSymbols, true)
  )

  def benchmarkSettings: Seq[Def.Setting[_]] = androidBuild ++ commonSettings ++ androidSettings ++ Seq(
    libraryDependencies ++= benchmarks, //++ tests,

    fork in run := true,

    proguardOptions in Android ++= Seq(
      "-keep class sta.tests.benchmarks.**",
      "-keepattributes InnerClasses",
      "-dontwarn com.google.monitoring.runtime.instrumentation.**",
      "-dontwarn sun.misc.Unsafe",
      "-dontwarn com.google.common.collect.MinMaxPriorityQueue"
    )
  )

  def appAndroidSettings: Seq[Def.Setting[_]] = androidBuild ++ commonSettings ++ robolectricSettings ++ androidSettings ++ stdLibs ++ lintingSettings

  def libAndroidSettings: Seq[Def.Setting[_]] = androidBuildApklib ++ commonSettings ++ robolectricSettings ++ androidSettings ++ stdLibs ++ lintingSettings

  private def excludeSources(in: File): Seq[File] = {
    if (in.isDirectory) in.listFiles(fileFilters.isScalaFile) ++ in.listFiles(fileFilters.isDirectory).flatMap(excludeSources)
    else Seq.empty
  }

  private def lintingSettings: Seq[Def.Setting[_]] = Seq(
//    scalacOptions ++= Seq(
//      "-deprecation",
//      "-encoding", "UTF-8", // yes, this is 2 args
//      "-feature",
//      "-unchecked",
//      "-Xfatal-warnings",
//      "-Xlint",
//      "-Yno-adapted-args",
//      "-Ywarn-numeric-widen",
//      "-Ywarn-value-discard",
//      "-Xfuture"
//    )
  ) ++ wartremoverSettings ++ Seq(
    wartremoverErrors ++= Seq(
      Wart.Any,
      Wart.Any2StringAdd,
      Wart.DefaultArguments,
      Wart.EitherProjectionPartial,
      Wart.JavaConversions,
      Wart.ListOps,
      Wart.MutableDataStructures,
      Wart.NoNeedForMonad,
      Wart.Nothing,
      Wart.Null,
      Wart.OptionPartial,
      Wart.Product,
      Wart.Return,
      Wart.Serializable,
      Wart.TryPartial,
      Wart.Var
      /*,
      Wart.AsInstanceOf, // FIXME refactor pattern matching
      Wart.IsInstanceOf, // FIXME refactor pattern matching
      Wart.NonUnitStatements, // often used due to fact of android api
      Wart.Throw // try/catch in some hot points
      */
    ),
    wartremoverExcluded ++= excludeSources(target.value / "android-gen") ++
      excludeSources(sourceDirectory.value / "test" / "scala")
    //    addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT"),
  ) ++ scalastyle.ScalastylePlugin.Settings ++ Seq(
    scalastyle.PluginKeys.failOnError := true
      //    compile in Compile <<= compile in Compile dependsOn org.scalastyle.sbt.PluginKeys.scalastyle.toTask(""),
      //    compile in Test <<= compile in Test dependsOn org.scalastyle.sbt.PluginKeys.scalastyle.toTask(""),
      //    org.scalastyle.sbt.PluginKeys.config := file("scalastyle-config.xml"),
      //    org.scalastyle.sbt.PluginKeys.scalastyleTarget := file("reports") / thisProject.value.id / "scalastyle-result.xml"
  )

  private def stdLibs: Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      scalaz.core,
      scalaz.concurrent
    ) ++ tests.map(_ % "test")
  )

  private def robolectricSettings: Seq[Def.Setting[_]] = Seq(
    resolvers += "RoboTest releases" at "https://raw.github.com/zbsz/mvn-repo/master/releases/",
    libraryDependencies ++= robolectricTests.map(_ % "test"),
    fork in Test := true,
    javaOptions in Test ++= Seq("-XX:MaxPermSize=2048M", "-XX:+CMSClassUnloadingEnabled")
  )

  private def androidSettings: Seq[Def.Setting[_]] = Seq(
    platformTarget in Android := versions.androidSDK,
    transitiveAndroidLibs in Android := false,
    debugIncludesTests in Android := false
  ) ++ stdProguardSettings

  private def stdProguardSettings = Seq (
    proguardOptions in Android ++= Seq(
      "-keep public class * extends junit.framework.TestCase",
      "-keepclassmembers class * extends junit.framework.TestCase { *; }",
      "-keepattributes Signature",
      "-dontwarn scala.collection.**",
      "-dontwarn scalaz.concurrent.**", // TODO narrow
      "-dontwarn spire.macros.**"
    ),

    proguardCache in Android ++= Seq(
      ProguardCache("org.parboiled2") % "org.parboiled" %% "parboiled",
      ProguardCache("scala.reflect") % "org.scala-lang" %% "scala-reflect",
      ProguardCache("scala") % "org.scala-lang.modules",
      ProguardCache("scalaz") % "org.scalaz",
      ProguardCache("scodec") % "org.scodec",
      ProguardCache("shapeless") % "com.chuusai",
      ProguardCache("spire") % "org.spire-math"
    )
  )
}
