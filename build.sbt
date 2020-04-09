// net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "de.active-group"
name := "ScalaJasper"

organizationHomepage := Some(url("http://www.active-group.de/"))

scalaVersion in ThisBuild := "2.13.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

// 2.10: scalacOptions ++= ["-feature", "-language:implicitConversions"]

crossScalaVersions := Seq("2.10.4", "2.11.2", "2.13.1")

compileOrder in Compile := CompileOrder.Mixed

pomIncludeRepository := { _ => false }
publishMavenStyle := true
publishArtifact in Test := false

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
useGpg := true

// patched IText version is only at http://jasperreports.sourceforge.net/maven2/com/lowagie/itext/2.1.7.js2/
resolvers += "JasperReports Repository" at "http://jasperreports.sourceforge.net/maven2/"

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


