import sbt.Project.projectToRef

val scalaV = "2.11.8"

lazy val server = project.settings(
  scalaVersion := scalaV,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  libraryDependencies ++= Seq (
    "com.lihaoyi" %% "upickle" % "0.4.3",
    "com.vmunier" %% "scalajs-scripts" % "1.0.0",
    "com.typesafe" % "config" % "1.2.1",
    "org.webjars" % "codemirror" % "5.11")
 ).enablePlugins(PlayScala,SbtWeb)
  .dependsOn(commonJVM, changeManagement, guidelineChecking)

lazy val client = project.dependsOn(common.js).settings(
    scalaVersion := scalaV,
    persistLauncher := true,
    persistLauncher in Test := false,
    unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value),
    resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases"),
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0",
      "com.lihaoyi" %%% "upickle" % "0.4.3",
      "org.denigma" %%% "codemirror-facade" % "5.11-0.7"
    )).enablePlugins(ScalaJSPlugin)

lazy val common = (crossProject.crossType(CrossType.Pure) in file("common"))
  .settings(
    scalaVersion := scalaV,
    libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.3"
  ).jsConfigure(_ enablePlugins ScalaJSWeb)

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