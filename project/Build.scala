import sbt._
import Keys._
import sbtassembly.Plugin._ 
import AssemblyKeys._

object JRLangBuild extends Build {

  // should be first, alphabetically
  lazy val JRLang = Project(id = "JRLang",
    base = file("."))

}
