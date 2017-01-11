name := "change-management"

version := "1.0"

scalaVersion := "2.11.8"

parallelExecution in Test := false

resolvers += Resolver.bintrayRepo("dfki-cps","maven")

resolvers ++= Seq(
  "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "de.dfki.cps" %% "stools" % "1.0.1",
  "de.dfki.cps" %% "guideline-checking" % "1.0.0",
  "de.dfki.cps" %% "specific-sysml" % "0.1.6",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "com.typesafe" % "config" % "1.2.1",
  "org.neo4j" % "neo4j" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

unmanagedSourceDirectories in Compile += baseDirectory.value / "emf" / "src"