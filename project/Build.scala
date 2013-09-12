import sbt._
import Keys._
import sbtassembly.Plugin._ 
import AssemblyKeys._

object ScalaJasperBuild extends Build {

  // should be first, alphabetically
  lazy val ScalaJasper = Project(
    id = "scalajasper",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      version := "0.2-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "1.9.1" % "test",
        "log4j" % "log4j" % "1.2.15"
          excludeAll(
            ExclusionRule(organization = "com.sun.jdmk", name = "jmxtools"),
            ExclusionRule(organization = "com.sun.jmx", name = "jmxri"),
            ExclusionRule(organization = "javax.jms", name = "jms")),
        "net.sf.jasperreports" % "jasperreports" % "5.1.0",
        "junit" % "junit" % "4.8" % "test",
        // see http://www.slf4j.org/codes.html#StaticLoggerBinder
        "org.slf4j" % "slf4j-log4j12" % "1.5.8"
      )
    )
  )

}
