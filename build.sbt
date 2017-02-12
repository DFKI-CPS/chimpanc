import sbt.Project.projectToRef

val scalaV = "2.11.8"

scalaVersion in ThisBuild := scalaV

lazy val server = (project in file("modules/server")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := Seq(client),
  isDevMode := true,
  pipelineStages in Assets := Seq(scalaJSPipeline),
  libraryDependencies ++= Seq (
    "com.lihaoyi" %% "upickle" % "0.4.3",
    "com.vmunier" %% "scalajs-scripts" % "1.0.0",
    "com.typesafe" % "config" % "1.2.1",
    "org.webjars" % "codemirror" % "5.11")
 ).enablePlugins(PlayScala,SbtWeb)
  .dependsOn(commonJVM, changeManagement)

lazy val client = (project in file("modules/client")).dependsOn(common.js).settings(
    scalaVersion := scalaV,
    (emitSourceMaps in fullOptJS) := true,
    persistLauncher := true,
    persistLauncher in Test := false,
    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0",
      "com.lihaoyi" %%% "upickle" % "0.4.3"
    )).enablePlugins(ScalaJSPlugin)

lazy val common = (crossProject.crossType(CrossType.Pure) in file("modules/common"))
  .settings(
    scalaVersion := scalaV,
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.3"
  ).jsConfigure(_ enablePlugins ScalaJSWeb)

lazy val changeManagement = project in file("modules/change-management") dependsOn(common.jvm)

lazy val commonJS = common.js
lazy val commonJVM = common.jvm

// loads the Play project at sbt startup
onLoad in Global :=
  (Command.process("project server", _: State)) compose (onLoad in Global).value