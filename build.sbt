import sbt.Project.projectToRef

val scalaV = "2.11.8"

lazy val jsProjects = Seq(client)

lazy val server = project.settings(
  scalaVersion := scalaV,
  scalaJSProjects := jsProjects,
  pipelineStages := Seq(scalaJSProd),
  libraryDependencies ++= Seq (
    "com.lihaoyi" %% "upickle" % "0.2.8",
    "com.vmunier" %% "play-scalajs-scripts" % "0.1.0",
    "com.typesafe" % "config" % "1.2.1",
    "org.webjars" % "codemirror" % "5.6")
 ).enablePlugins(PlayScala)
  .aggregate(jsProjects.map(projectToRef): _*)
  .dependsOn(commonJVM, changeManagement, guidelineChecking)

lazy val graph = project in file("graph")

lazy val client = project.dependsOn(common.js).settings(
    scalaVersion := scalaV,
    persistLauncher := true,
    persistLauncher in Test := false,
    sourceMapsDirectories += commonJS.base / "..",
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    resolvers += bintray.Opts.resolver.repo("denigma", "denigma-releases"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0",
      "com.lihaoyi" %%% "upickle" % "0.2.8",
      "org.scalajs" %%% "codemirror" % "4.8-0.4"
    )).enablePlugins(ScalaJSPlugin, ScalaJSPlay)

lazy val common = (crossProject.crossType(CrossType.Pure) in file("common"))
  .settings(scalaVersion := scalaV)
  .jsConfigure(_ enablePlugins ScalaJSPlay)
  .jsSettings(
    sourceMapsBase := baseDirectory.value / "..",
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.2.8")
  .jvmSettings(
    libraryDependencies += "com.lihaoyi" %% "upickle" % "0.2.8")

lazy val guidelineChecking = project.in(file("guideline-checking")).settings(
  scalaVersion := scalaV,
  libraryDependencies ++= Seq(
    "edu.stanford.nlp" % "stanford-parser" % "3.3.1",
    "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
    "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models")
)

lazy val changeManagement = project in file("change-management") dependsOn(common.jvm, guidelineChecking)

lazy val commonJS = common.js
lazy val commonJVM = common.jvm

// loads the Play project at sbt startup
onLoad in Global :=
  (Command.process("project server", _: State)) compose (onLoad in Global).value

EclipseKeys.skipParents in ThisBuild := false