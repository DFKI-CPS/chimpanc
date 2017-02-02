package specific


import collection.JavaConverters._
import de.dfki.cps.stools.STools
import java.io.File

import de.dfki.cps.egraph.{EGraphStore, GraphResource, Labels}
import de.dfki.cps.secore.SResource
import de.dfki.cps.specific.nlp
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import de.dfki.cps.egraph.internal.Util._

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.Position



object ChangeManagement  {
  implicit val resourceSet = {
    val rs = new ResourceSetImpl
    de.dfki.cps.specific.sysml.Synthesis.prepareLibrary(rs)
    rs
  }

  val dbFile = new File("db")
  if (!dbFile.exists()) dbFile.mkdirs()
  assert(dbFile.isDirectory)

  val store = new EGraphStore(dbFile)
  store.attach(resourceSet)

  implicit val output = new Output.MultiOut
  output.add(Output.StdOut)

  private var layerObjects = Map.empty[String,Set[LayerObject]]

  private val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))

  private var nlProblems = Set.empty[RequirementEvaluation]

  def listIssues() = {
    /*val out = output.task("Detecting semantic problems")
    val problems = Semantics.findProblems() ++ project.layers.zip(project.layers.tail).flatMap {
      case (to,from) =>
        val f = Semantics.constraints(from.name)
        if (f.nonEmpty) Semantics.constraints(to.name).map {
          case (cl, constr, proven) =>
            OCLProofObligation(to.name, cl.name, f.map(_._2.name), constr.name, proven)
        } else Seq.empty
    }
    out.taskDone(IssueList(nlProblems ++ problems))*/
  }

  val positions = mutable.Map.empty[(String,String),Position]

  def proven(layer: String, constr: String) = ??? // Semantics.proven(layer,constr)

  def entities = layerObjects

  def evaluateNL(layer: Resource) = {
    val out = output.task(s"Evaluating NL layer '${layer.getURI}'")
    val reqs = layer.getAllContents.asScala.collect {
      case r: org.eclipse.papyrus.sysml.requirements.Requirement =>
        r
    }
    layerObjects += layer.getURI.toString -> reqs.map { r =>
      val pos = positions((layer.getURI.toString,layer.getURIFragment(r.getBase_Class)))
      LayerObject(r.getId, r.getText,pos.line,pos.column)
    }.toSet
    nlProblems = Set.empty
    val max = reqs.length
    out.progress(0)
    var n = 0
    reqs.foreach { req =>
      val line = nlp.Pipeline.annotate(req.getText)
      import nlp.Rules._
      val eval = RequirementEvaluation(layer.getURI.toString, layer.getURIFragment(req), List(
        ruleIsOneRequirement(line),
        ruleHasNoConjunctions(line),
        ruleIsDirectSentence(line),
        ruleIsRequirementFormat(line),
        ruleAvoidLetOutClauses(line),
        ruleAvoidExpressingSuggestions(line),
        ruleAvoidWeakPhrases(line),
        ruleAvoidSpeculation(line),
        ruleAvoidWishfulThinking(line),
        ruleVerifiableCriteria(line).orElse(Some(true))
      ))
      if (eval.results.exists(_.contains(false)))
        nlProblems += eval
      n += 1
      out.progress((n * 1000) / max)
    }
    out.taskDone()
  }

  /*def commitSysML(layer: Resource) = {
    implicit val out = output.task(s"Committing SysML layer '${layer.name}'")

    val oldModel = resourceSet.getResource(URI.createURI("graph:://" + layer.name), true)

    out.info("Parsing SysML")
    val newModel = Try {
      val resource = resourceSet.createResource(URI.createFileURI(File.createTempFile("temp",".ecore").getPath))
      de.dfki.cps.specific.SysML.load()
    }

    if (newModel.isFailure) {
      newModel.failed.get match {
        case s => out.error(s"Failed to load SysML model: " + s.getMessage, newModel.failed.get)
      }
    }

    for {
      newModel <- newModel
    } {
      //newModel.save(mapAsJavaMap(Map.empty))

      /*val entities = SysML.getEntities(newModel).map {
        case Clazz(n,_) => Clazz(n)
        case other => other
      }

      layerObjects = layerObjects + (layer.name -> entities.toSet)*/

      out.info("Creating syntactic diff")
      val diff = stools.getSTool("ecore").sdiff(new SResource(oldModel), new SResource(newModel))

      if (diff.isEmpty) {
        out.info("No changes")
        //Semantics.deleteDeleted(oldModel)
        //Semantics.resetAdded(oldModel)
      }
      else {
        if (diff.entries.size == 1 && diff.entries.head._1.getLabel() == "root") {
          out.info("No prior model; creating new one")
          newModel.save(new java.util.HashMap)
        }
        else {
          out.info("Resetting Semantics")
          //Semantics.deleteDeleted(oldModel)
          //Semantics.resetSemantics(oldModel)
          out.info("Applying diff")
          //applyDiff(diff)
        }

        val newGraphModel = resourceSet.getResource(URI.createURI("graph:://" + layer.name), true)

        out.info("Propagate FSL semantics")
        //Semantics.propagate(newGraphModel)
      }

      out.taskDone(Entities(layer.name, entities.toSet))
    }
  }*/

  /*def commitESL(layer: Layer.ESL) = {
    implicit val out = output.task(s"Committing ESL layer '${layer.name}'")

    out.info("Extracting ECore")

    val oldImpl = resourceSet.getResource(URI.createURI("graph:://" + layer.name), true)
    oldImpl.unload()
    val newImpl = ClangASTParser.parse(layer.file)  // ClangAstParser.parse(layer.file)

    newImpl.failed.foreach { e =>
      out.error("Failed to load ESL", e)
    }

    newImpl.foreach { newImpl =>
      val entities = SystemC.getEntities(newImpl).toSet
      layerObjects = layerObjects + (layer.name -> entities)

      out.info("Creating diff")

      val diff = stools.getSTool("ecore").sdiff(new SResource(oldImpl), new SResource(newImpl))
      if (diff.isEmpty) {
        out.info("No changes.")
        //Semantics.deleteDeleted(oldImpl)
        //Semantics.resetAdded(oldImpl)
      }
      else {
        if (diff.entries.size == 1 && diff.entries.head._1.getLabel() == "root") {
          out.info("No prior ESL entry; creating new one.")
          val res = Try(newImpl.save(new java.util.HashMap))
          res.failed.foreach { e =>
            out.error("Model could not be written to Graph", e)
          }
        }
        else {
          out.info("Resetting semantics")
          //Semantics.deleteDeleted(oldImpl)
          //Semantics.resetSemantics(oldImpl)
          out.info("Applying diff")
          //applyDiff(diff)
        }

        val newGraphImpl = resourceSet.getResource(URI.createURI("graph:://" + layer.name), true)

        out.info("Propagating ESL semantics")
        //Semantics.propagate(newGraphImpl)
      }

      out.taskDone(Entities(layer.name, entities))
    }
  }*/

  def load(output: TaskOutput): (URI => Resource) = (uri: URI) => {
    output.info(s"loading $uri")
    val file = new File(uri.toFileString)
    val graphURI = URI.createURI(s"graph://${uri.lastSegment()}") // FIXME
    println(graphURI)
    store.graphDb.transaction {
      Option(store.graphDb.findNode(Labels.Resource,"originalURI",uri.toString)).fold {
        // resource not existent before
        println("loading", uri)
        val resource = resourceSet.createResource(graphURI).asInstanceOf[GraphResource]
        val positions = de.dfki.cps.specific.SysML.load(file,resource,includeProfileApplcations = false)
        resource.save(new java.util.HashMap)
        resource.root.foreach(_.setProperty("originalURI",uri.toString))
        resource.root.foreach(_.setProperty("lastModified",file.lastModified()))
        resource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
        layerObjects += uri.toString -> sysml.SysML.getEntities(resource,positions.map {
          case (o,p) => resource.getURIFragment(o) -> p
        }.lift)
        resource
      } { node =>
        if (node.getProperties("lastModified") == file.lastModified()) {
          // wasn't modified
          resourceSet.getResource(graphURI, true).asInstanceOf[GraphResource]
        } else {
          // was potentially modified
          val oldResource = resourceSet.getResource(graphURI,true).asInstanceOf[GraphResource]
          val newResource = resourceSet.createResource(uri.appendFileExtension("ecore"))
          de.dfki.cps.specific.SysML.load(file,newResource)
          val diff = de.dfki.cps.secore.stools.getSTool("specific").sdiff(new SResource(oldResource), new SResource(newResource))
          de.dfki.cps.egraph.stools.Diff.applyDiff(oldResource,diff)
          oldResource.unload()
          oldResource.load(new java.util.HashMap)
          oldResource.root.foreach(_.setProperty("lastModified",file.lastModified()))
          oldResource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
          oldResource
        }
      }
    }.get
  }

  def layers = store.graphDb.transaction {
    val nodes = store.graphDb.findNodes(Labels.Resource)
    nodes.asScala.toList.foreach(println)
    Specs(store.graphDb.findNodes(Labels.Resource).asScala.map { node =>
      SysML(node.getProperty("originalURI").asInstanceOf[String],node.getProperty("content").asInstanceOf[String])
    }.toList)
  }.get

  def commit(): Unit = {
    val out = output.task("Updating")
    val uri = URI.createFileURI("example/index.sysml")
    val newResource = resourceSet.createResource(uri.appendFileExtension("ecore"))
    val res = de.dfki.cps.specific.SysML.loadProject(URI.createFileURI("example/index.sysml"),newResource,load(out))
    out.taskDone()
  }

  def createMapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject) = ()
    //Semantics.createMapping(fromLayer,from,toLayer,to)

  def removeMappings(layer: String, to: LayerObject) = ()
    //Semantics.removeMappings(layer, to)

  def ignoreModel(layer: String, model: LayerObject, reason: String) = {
    //Semantics.ignoreModel(layer, model, reason)
  }

  def listMappings() = {
    val out = output.task("Retrieving mappings from semantic graph")
    val result = Nil // Semantics.listMappings()
    out.taskDone(MappingsList(result.toSet))
  }
}