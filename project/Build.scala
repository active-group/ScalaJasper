import sbt._
import Keys._
import sbtassembly.Plugin._ 
import AssemblyKeys._

object ScalaJasperBuild extends Build {

  // should be first, alphabetically
  lazy val ScalaJasper = Project(id = "scalajasper",
    base = file("."))

}
