// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// voir http://www.wartremover.org/
lazy val warts = {
  import Wart._
  Warts.allBut(Any, NonUnitStatements, Nothing, Recursion)  
}

lazy val scala2_13 = "2.13.0"
lazy val scala2_12 = "2.12.10"
lazy val supportedScalaVersions = List(scala2_13, scala2_12)

lazy val globalSettings: Seq[sbt.Def.SettingsDefinition] =
  Seq(
    inThisBuild(
      List(
        organization := "chrilves",
        version := "0.1.0-SNAPSHOT",
        scalaVersion := "2.13.0",
        crossScalaVersions := supportedScalaVersions
      )),
    updateOptions := updateOptions.value.withCachedResolution(true),
    wartremoverErrors in (Compile, compile) := warts,
    wartremoverWarnings in (Compile, console) := warts,
    addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
    scalafmtOnCompile := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % Versions.cats,
      "org.scalatest" %%% "scalatest" % "3.0.8" % Test
    )
  )

lazy val autobatch =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .settings(globalSettings : _*)
    .settings(name := "auto-batch")

lazy val autobatchJS = autobatch.js
lazy val autobatchJVM = autobatch.jvm

lazy val root =
    project
      .in(file("."))
      .settings(globalSettings : _*)
      .dependsOn(autobatchJS, autobatchJVM)
      .aggregate(autobatchJS, autobatchJVM)

