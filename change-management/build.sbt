name := "change-management"

version := "1.0"

scalaVersion := "2.11.6"

parallelExecution in Test := false

resolvers ++= Seq(
  "anormcypher" at "http://repo.anormcypher.org/",
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.antlr" % "antlr-runtime" % "3.5.2",
  "org.jetbrains" % "annotations" % "13.0",
  "com.typesafe" % "config" % "1.2.1",
  "org.neo4j" % "neo4j" % "2.3.0-M03",
  "org.neo4j.app" % "neo4j-server" % "2.3.0-M03",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.eclipse.xtend" % "org.eclipse.xtend.lib" % "2.8.2",
  "edu.stanford.nlp" % "stanford-parser" % "3.3.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.3.1" classifier "models"
  //"org.eclipse.emf" % "org.eclipse.emf.common" % "2.10.0-v20140514-1158",
  //"org.eclipse.emf" % "org.eclipse.emf.ecore" % "2.10.0-v20140514-1158",
  //"org.eclipse.emf" % "org.eclipse.emf.ecore.xmi" % "2.10.0-v20140514-1158"
)

unmanagedSourceDirectories in Compile += baseDirectory.value / "emf" / "src"