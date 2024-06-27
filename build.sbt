ThisBuild / scalaVersion := "3.3.3"

organization := "de.active-group"
name := "ScalaJasper"
version := "0.6.0-SNAPSHOT"

organizationHomepage := Some(url("http://www.active-group.de/"))
homepage := Some(url("https://github.com/active-group/ScalaJasper"))
startYear := Some(2013)
description := "ScalaJasper is a purely functional, composable API for creating reports with JasperReports."
licenses += "Eclipse Public License either version 1.0 or (at your option) any later version." -> url("https://github.com/active-group/ScalaJasper/blob/master/LICENSE")
scmInfo := Some(ScmInfo(
  browseUrl=url("http://github.com/active-group/ScalaJasper"),
  connection="scm:git:git://github.com/active-group/ScalaJasper.git",
  devConnection=Some("scm:git:git@github.com:active-group/ScalaJasper.git")))

pomExtra := (<developers>
  <developer>
    <id>dfrese</id>
    <name>David Frese</name>
  </developer>
  <developer>
    <id>hdobretzberger</id>
    <name>Helmut Dobretzberger</name>
  </developer>
</developers>)

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

crossScalaVersions := Seq("3.3.3")

Compile / compileOrder := CompileOrder.Mixed

pomIncludeRepository := { _ => false }
publishMavenStyle := true
Test / publishArtifact := false

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

// patched IText version is only at https://jasperreports.sourceforge.net/maven2/com/lowagie/itext/2.1.7.js2/
resolvers += "JasperReports Repository" at "https://jasperreports.sourceforge.net/maven2/"

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


resolvers += "Jasper" at "https://jasperreports.sourceforge.net/maven2"
resolvers += "Jasper Third Party" at "https://jaspersoft.jfrog.io/jaspersoft/third-party-ce-artifacts"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.23.1"
libraryDependencies += "net.sf.jasperreports" % "jasperreports" % "6.12.2" // fixme: higher is not possible, results in an error
libraryDependencies += "junit" % "junit" % "4.13.2" % "test"
libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "2.0.13"

assembly / assemblyMergeStrategy := {
  case PathList("javax", "xml", _*) => MergeStrategy.first
  case PathList("META-INF", "spring.tooling") => MergeStrategy.discard
  case "overview.html" => MergeStrategy.discard
  case other => MergeStrategy.first
}