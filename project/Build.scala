import sbt._
import sbt.ExclusionRule
import sbt.Keys._
import sbtassembly.Plugin._ 
import AssemblyKeys._

object ScalaJasperBuild extends Build {

  // should be first, alphabetically
  lazy val ScalaJasper = Project(
    id = "scalajasper",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      version := "0.4.1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.1" % "test",
        "log4j" % "log4j" % "1.2.15"
          excludeAll(
            ExclusionRule(organization = "com.sun.jdmk", name = "jmxtools"),
            ExclusionRule(organization = "com.sun.jmx", name = "jmxri"),
            ExclusionRule(organization = "javax.jms", name = "jms")),
        "net.sf.jasperreports" % "jasperreports" % "5.1.0",
        "junit" % "junit" % "4.8" % "test",
        // see http://www.slf4j.org/codes.html#StaticLoggerBinder
        "org.slf4j" % "slf4j-log4j12" % "1.5.8"
      ),
      homepage := None,
      startYear := Some(2013),
      description := "ScalaJasper is a purely functional, composable API for creating reports with JasperReports.",
      licenses += "BSD 3-Clause" -> url("https://github.com/active-group/ScalaJasper/blob/master/LICENSE"),
      scmInfo := Some(ScmInfo(
        browseUrl=url("http://github.com/active-group/ScalaJasper"),
        connection="scm:git:git://github.com/active-group/ScalaJasper.git",
        devConnection=Some("scm:git:git@github.com:active-group/ScalaJasper.git"))),
      pomExtra := (<developers>
        <developer>
          <id>dfrese</id>
          <name>David Frese</name>
        </developer>
      </developers>)
    )
  )

}
