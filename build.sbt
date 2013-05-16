import AssemblyKeys._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

assemblySettings

net.virtualvoid.sbt.graph.Plugin.graphSettings

organization := "Active Group"

scalaVersion in ThisBuild := "2.9.2"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

crossPaths := false

compileOrder in Compile := CompileOrder.Mixed

libraryDependencies in JRLang ++= Seq(
    "org.scalatest" %% "scalatest" % "1.9.1" % "test",
    "log4j" % "log4j" % "1.2.15"
        excludeAll(
             ExclusionRule(organization = "com.sun.jdmk"),
             ExclusionRule(organization = "com.sun.jmx"),
             ExclusionRule(organization = "javax.jms")),
    "net.sf.jasperreports" % "jasperreports" % "5.1.0", // newest
    //"net.sf.jasperreports" % "jasperreports" % "4.0.1", // equals uses this
    // exclude("commons-beanutils", "commons-beanutils"),
    //"net.sf.jasperreports" % "jasperreports" % "4.0.1" exclude("commons-beanutils", "commons-beanutils"),
    "junit" % "junit" % "4.8" % "test",
    // see http://www.slf4j.org/codes.html#StaticLoggerBinder
    "org.slf4j" % "slf4j-log4j12" % "1.5.8"
)


// patched IText version is only at http://jasperreports.sourceforge.net/maven2/com/lowagie/itext/2.1.7.js2/
resolvers += "JasperReports Repository" at "http://jasperreports.sourceforge.net/maven2/"
