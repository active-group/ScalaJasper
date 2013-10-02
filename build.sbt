import AssemblyKeys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "de.activegroup"

organizationHomepage := Some(url("http://www.active-group.de/"))

scalaVersion in ThisBuild := "2.9.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

crossPaths := false

compileOrder in Compile := CompileOrder.Mixed

// patched IText version is only at http://jasperreports.sourceforge.net/maven2/com/lowagie/itext/2.1.7.js2/
resolvers += "JasperReports Repository" at "http://jasperreports.sourceforge.net/maven2/"

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
