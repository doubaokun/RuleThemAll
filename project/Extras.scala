import sbt.Keys._
import sbt._

object Extras {
  sealed trait ExternalJar {
    def filename: String
    
    def url: URL
  }
  
  case class GithubRelease(repo: String, tag: String, filename: String) extends ExternalJar {
    def url: URL = new URL(s"https://github.com/$repo/releases/download/$tag/$filename")
  }

  val externalJars = SettingKey[Seq[ExternalJar]]("externalJars",
    "Contains list of external jars that should be fetched before resolving unmanaged jars.")

  val fetchExternalJars = TaskKey[Unit]("fetchExternalJars", "Fetch external jars.")

  def externalJarsSettings: Seq[Def.Setting[_]] = inConfig(Compile)(Seq(
    externalJars := Seq.empty,
    fetchExternalJars := {
      externalJars.value.foreach { external =>
        val lib = unmanagedBase.value / external.filename
        if (!lib.exists()) {
          IO.download(external.url, lib)
        }
      }
    },
    unmanagedJars <<= unmanagedJars dependsOn fetchExternalJars
  ))
}
