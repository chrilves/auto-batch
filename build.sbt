// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// voir http://www.wartremover.org/
lazy val warts =
  Warts.allBut(Wart.Any, Wart.NonUnitStatements, Wart.Nothing, Wart.Recursion)  

lazy val globalSettings: Seq[sbt.Def.SettingsDefinition] =
  Seq(
    inThisBuild(
      List(
        organization := "com.example",
        scalaVersion := "2.12.8",
        version := "0.1.0-SNAPSHOT"
      )),
    updateOptions := updateOptions.value.withCachedResolution(true),
    wartremoverErrors in (Compile, compile) := warts,
    wartremoverWarnings in (Compile, console) := warts,
    addCompilerPlugin("io.tryp" % "splain" % "0.4.1" cross CrossVersion.patch),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.10" cross CrossVersion.binary),
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

