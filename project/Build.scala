import sbt.Keys._
import sbt.{ExclusionRule, _}
import sbtassembly.AssemblyKeys.assembly
import sbtassembly.{AssemblyKeys, MergeStrategy, PathList}

object ScalaJasperBuild extends Build {

  // should be first, alphabetically
  lazy val ScalaJasper = Project(
    id = "scalajasper",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      version := "0.5.9",
      fork := true,
      resolvers += "Jasper" at "http://jasperreports.sourceforge.net/maven2",
      resolvers += "Jasper Third Party" at "https://jaspersoft.jfrog.io/jaspersoft/third-party-ce-artifacts",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.2.0-M2" % "test",
        "org.apache.logging.log4j" % "log4j-core" % "2.14.1",
        "net.sf.jasperreports" % "jasperreports" % "6.12.2", // fixme: higher is not possible, results in an error
        "junit" % "junit" % "4.8" % "test",
        // see http://www.slf4j.org/codes.html#StaticLoggerBinder
        "org.slf4j" % "slf4j-log4j12" % "1.7.32"
      ),
      AssemblyKeys.assemblyMergeStrategy in assembly <<= (AssemblyKeys.mergeStrategy in assembly) { (old: String => MergeStrategy) => { s: String => s match
      {
        case PathList("javax", "xml", xs @ _*) => MergeStrategy.first
        case PathList("META-INF", "spring.tooling") => MergeStrategy.discard
        case "overview.html" => MergeStrategy.discard
        case x => old(x)
      }
      }},
      homepage := Some(url("https://github.com/active-group/ScalaJasper")),
      startYear := Some(2013),
      description := "ScalaJasper is a purely functional, composable API for creating reports with JasperReports.",
      licenses += "Eclipse Public License either version 1.0 or (at your option) any later version." -> url("https://github.com/active-group/ScalaJasper/blob/master/LICENSE"),
      scmInfo := Some(ScmInfo(
        browseUrl=url("http://github.com/active-group/ScalaJasper"),
        connection="scm:git:git://github.com/active-group/ScalaJasper.git",
        devConnection=Some("scm:git:git@github.com:active-group/ScalaJasper.git"))),
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
    )
  )
}

