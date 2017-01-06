package specific

import org.eclipse.emf.ecore._
import org.eclipse.ocl.SemanticException
import graph.ECoreToGraph._
import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil
import nl.Requirements
import project.{Layer, Project}
import specific.emf.cpp.CppPackage
import specific.systemc.{SystemC, ClangASTParser}
import sysml.{OCL, Emfatic}
import specific.graph.{Neo4j, Semantics, HybridResourceSet}
import secore.SEcore._
import collection.JavaConversions._
import de.dfki.cps.stools.STools
import java.io.File
import org.eclipse.papyrus.sysml.blocks.BlocksPackage
import org.eclipse.emf.ecore.xmi.impl.EcoreResourceFactoryImpl
import de.dfki.cps.specific.nlp

object ChangeManagement  {
  private var initialized = false
  def initialize() = if (!initialized) {
    val out = output.task("Initializing")
    initialized = true
    out.info("Clear Database")
    clear()
    out.taskDone()
    commit()
  } else commit()

  implicit val output = new Output.MultiOut
  output.add(Output.StdOut)

  def project = Project.parse("files/project")

  implicit val database = Neo4j("db")
      
  implicit lazy val resourceSet = {
    val rs = new HybridResourceSet
    UMLResourcesUtil.init(rs)
    rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
    rs.getPackageRegistry put ("http://www.eclipse.org/emf/2002/Ecore", EcorePackage.eINSTANCE)
    rs.getPackageRegistry put ("http://www.dfki.de/SPECifIC/C++", CppPackage.eINSTANCE)
    rs.getResourceFactoryRegistry.getExtensionToFactoryMap.put("ecore", new EcoreResourceFactoryImpl())
    rs.getResourceFactoryRegistry.getExtensionToFactoryMap.put("xmi", new EcoreResourceFactoryImpl())
    rs
  }

  private var layerObjects = Map.empty[String,Set[LayerObject]]

  private val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))     

  private var nlProblems = Set.empty[RequirementEvaluation]

  def listIssues() = {
    val out = output.task("Detecting semantic problems")
    val problems = Semantics.findProblems() ++ project.layers.zip(project.layers.tail).flatMap {
      case (to,from) =>
        val f = Semantics.constraints(from.name)
        if (f.nonEmpty) Semantics.constraints(to.name).map {
          case (cl, constr, proven) =>
            OCLProofObligation(to.name, cl.name, f.map(_._2.name), constr.name, proven)
        } else Seq.empty
    }
    out.taskDone(IssueList(nlProblems ++ problems))
  }

  def proven(layer: String, constr: String) = Semantics.proven(layer,constr)

  def entities = layerObjects

  def evaluateNL(layer: Layer.NL) = {
    val out = output.task(s"Evaluating NL layer '${layer.name}'")
    val reqs = Requirements.load(layer.file)
    layerObjects += layer.name -> reqs.toSet
    nlProblems = Set.empty
    val max = reqs.length
    out.progress(0)
    var n = 0
    reqs.foreach { req =>
      val line = nlp.Pipeline.annotate(req.content)
      import nlp.Rules._
      val eval = RequirementEvaluation(layer.name, req, List(
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

  def commitNL(layer: Layer.NL) = {
    val out = output.task(s"Committing NL Layer '${layer.name}'")
    out.info("Parsing requirements")
    val reqs = Requirements.load(layer.file)
    out.info("Writing Requirements to semantic graph")
    Requirements.writeToGraph(layer.name, reqs)
    layerObjects += layer.name -> reqs.toSet[LayerObject]
    out.taskDone(Entities(layer.name, reqs.toSet))
  }

  def commitFSL(layer: Layer.FSL) = {
    implicit val out = output.task(s"Committing FSL layer '${layer.name}'")

    val oldModel = resourceSet.getGraphResource(layer.name, loadOnDemand =  false)

    out.info("Parsing EMFatic and OCL")
    val newModel = Emfatic.load(layer.emfFile)
    val newOclModel = newModel.flatMap(newModel => OCL.load(layer.oclFile, newModel))


    if (newModel.isFailure) {
      newModel.failed.get match {
        case s => out.error(s"Failed to load EMFactic model: " + s.getMessage, newModel.failed.get)
      }
    }
    else if (newOclModel.isFailure) {
      newOclModel.failed.get match {
        case s: SemanticException => out.error(s"Inconsistent OCL", s)
        case s => out.error(s"Failed to read OCL: " + s.getMessage, s)
      }
    }

    for {
      newModel <- newModel
      newOclModel <- newOclModel
    } {
      newModel.save(mapAsJavaMap(Map.empty))

      val entities = Emfatic.getEntities(newModel).map {
        case Clazz(n,_) => Clazz(n,layer.getStateChart(n))
        case other => other
      }

      layerObjects = layerObjects + (layer.name -> (entities.toSet ++ OCL.getInfos(newOclModel)))

      out.info("Creating syntactic diff")
      val diff = stools.getSTool("ecore").sdiff(new SResource(oldModel, layer.name), new SResource(newModel, layer.name))

      if (diff.isEmpty) {
        out.info("No changes")
        Semantics.deleteDeleted(oldModel)
        Semantics.resetAdded(oldModel)
      }
      else {
        if (diff.entries.size == 1 && diff.entries.head._1.getLabel() == "root") {
          out.info("No prior FSL model; creating new one")
          database.transaction { implicit tx =>
            writeResource(newModel, layer.name)
          }
        }
        else {
          out.info("Resetting Semantics")
          Semantics.deleteDeleted(oldModel)
          Semantics.resetSemantics(oldModel)
          out.info("Applying diff")
          applyDiff(diff)
        }

        val newGraphModel = resourceSet.getGraphResource(layer.name, loadOnDemand =  false)

        out.info("Propagate FSL semantics")
        Semantics.propagate(newGraphModel)
      }

      out.taskDone(Entities(layer.name, entities.toSet ++ OCL.getInfos(newOclModel)))
    }
  }

  def commitESL(layer: Layer.ESL) = {
    implicit val out = output.task(s"Committing ESL layer '${layer.name}'")

    out.info("Extracting ECore")

    val oldImpl = resourceSet.getGraphResource(layer.name, loadOnDemand = false)
    oldImpl.unload()
    val newImpl = ClangASTParser.parse(layer.file)  // ClangAstParser.parse(layer.file)

    newImpl.failed.foreach { e =>
      out.error("Failed to load ESL", e)
    }

    newImpl.foreach { newImpl =>
      val entities = SystemC.getEntities(newImpl).toSet
      layerObjects = layerObjects + (layer.name -> entities)

      out.info("Creating diff")

      val diff = stools.getSTool("ecore").sdiff(new SResource(oldImpl, layer.name), new SResource(newImpl, layer.name))
      if (diff.isEmpty) {
        out.info("No changes.")
        Semantics.deleteDeleted(oldImpl)
        Semantics.resetAdded(oldImpl)
      }
      else {
        if (diff.entries.size == 1 && diff.entries.head._1.getLabel() == "root") {
          out.info("No prior ESL entry; creating new one.")
          val res = database.transaction { implicit tx =>
            writeResource(newImpl, layer.name)
          }
          res.failed.foreach { e =>
            out.error("Model could not be written to Graph", e)
          }
        }
        else {
          out.info("Resetting semantics")
          Semantics.deleteDeleted(oldImpl)
          Semantics.resetSemantics(oldImpl)
          out.info("Applying diff")
          applyDiff(diff)
        }

        val newGraphImpl = resourceSet.getGraphResource(layer.name, loadOnDemand =  false)

        out.info("Propagating ESL semantics")
        Semantics.propagate(newGraphImpl)
      }

      out.taskDone(Entities(layer.name, entities))
    }
  }

  def autoMap(): Unit = {
    val out = output.task("Autodetecting mappings")
    Semantics.autoMap()
    out.taskDone()
  }

  def commit(layer: String): Unit = {
    project.layers.find(_.name == layer) match {
      case Some(nl:  Layer.NL)  => commitNL(nl)
      case Some(fsl: Layer.FSL) => commitFSL(fsl)
      case Some(esl: Layer.ESL) => commitESL(esl)
    }
  }

  def commit(): Unit = {
    project.layers.foreach(l => commit(l.name))
    project.layers.tail.zip(project.layers).foreach { case (from,to) =>
      Semantics.createMapping(from.name, to.name)
    }
  }

  def clear(): Unit = resourceSet.clearGraph()

  def createMapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject) =
    Semantics.createMapping(fromLayer,from,toLayer,to)

  def removeMappings(layer: String, to: LayerObject) =
    Semantics.removeMappings(layer, to)

  def ignoreModel(layer: String, model: LayerObject, reason: String) = {
    Semantics.ignoreModel(layer, model, reason)
  }

  def listMappings() = {
    val out = output.task("Retrieving mappings from semantic graph")
    val result = Semantics.listMappings()
    out.taskDone(MappingsList(result.toSet))
  }
}