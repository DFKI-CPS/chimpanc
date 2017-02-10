package specific


import collection.JavaConverters._
import de.dfki.cps.stools.STools
import java.io.File

import de.dfki.cps.egraph.{EGraphStore, GraphResource, Labels}
import de.dfki.cps.secore.{SAttribute, SObject, SReference, SResource}
import de.dfki.cps.specific.nlp
import org.eclipse.uml2.uml
import org.eclipse.papyrus.sysml.requirements
import org.eclipse.emf.common.util.URI
import org.eclipse.emf.ecore.resource.Resource
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl
import de.dfki.cps.egraph.internal.Util._
import de.dfki.cps.stools.editscript.{SEditScript, UpdateAnnotation}
import org.eclipse.uml2.uml.OpaqueExpression
import specific.systemc.ClangASTParser

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.input.{NoPosition, Position}



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

  def getConstraints(model: uml.Model) = {
    model.eAllContents().asScala.collect {
      case c: uml.Constraint => c
    }
  }

  def calcualateProofObligations() = {
    val out = output.task("Calculating Proof Obligations")
    val project = resourceSet.getResource(URI.createURI("graph://index"),true)
    val mappedConstraints = project.getAllContents.asScala.collect {
      case c: uml.Constraint if !c.getConstrainedElements.isEmpty => c.getConstrainedElements.get(0) -> c
    }.toMap
    val obligations: Set[SemanticIssue] = project.getAllContents.asScala.collect {
      case r: uml.Realization =>
        (r.getSuppliers.get(0),r.getClients.asScala.headOption) match {
          case (sm: uml.Model, Some(cm: uml.Model)) => {
            getConstraints(sm).map { c =>
              val pos = store.graphDb.transaction {
                positions(c.eResource().asInstanceOf[GraphResource].root.get.getProperty("originalURI").asInstanceOf[String]).get(c.eResource().getURIFragment(c.getSpecification))
              }.get.getOrElse(NoPosition)
              val ctx = c.getContext match {
                case op: uml.Operation => op.getClass_
                case prop: uml.Property => prop.getClass_
                case other => other
              }
              val spec = mappedConstraints.get(c).getOrElse(c).getSpecification.asInstanceOf[OpaqueExpression].getBodies.get(0)
              OCLProofObligation(normalizeURI(ctx.eResource().getURI).toString,ctx.eResource().getURIFragment(ctx),spec,false,pos.line,pos.column)
            }.toSeq
          }
          case other => Nil
        }
    }.flatten.toSet
    out.taskDone(IssueList(obligations ++ issues.values.flatten))
  }

  def listIssues() = {
    calcualateProofObligations()

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

  val positions = mutable.Map.empty[String,Map[String,Position]]
  val issues = mutable.Map.empty[String,Set[SemanticIssue]]
  val mappings = mutable.Map.empty[String,Set[Mapping]]

  def updateIssues(layer: String, diff: SEditScript): Unit = {
    issues(layer) = diff.entries.flatMap {
      case (o: SAttribute, entry) =>
        o.parent.underlying match {
          case named: uml.NamedElement =>
            println("changed: " + named.eResource().getURIFragment(named))
            mappings.values.flatMap(_.find(x => x.fromLayer == layer && x.from == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedClient(tl,t))
            } ++
            mappings.values.flatMap(_.find(x => x.toLayer == layer && x.to == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedSupplier(fl,f))
            } :+ Modified(layer, named.eResource().getURIFragment(named))
          case other => Seq.empty
        }
      case (o: SObject, entry) =>
        o.underlying match {
          case named: uml.NamedElement =>
            println("changed: " + named.eResource().getURIFragment(named))
            mappings.values.flatMap(_.find(x => x.fromLayer == layer && x.from == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedClient(tl,t))
            } ++
            mappings.values.flatMap(_.find(x => x.toLayer == layer && x.to == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedSupplier(fl,f))
            } :+ Modified(layer, named.eResource().getURIFragment(named))
          case other => Seq.empty
        }
      case (o: SReference, entry) =>
        o.parent.underlying match {
          case named: uml.NamedElement =>
            println("changed: " + named.eResource().getURIFragment(named))
            mappings.values.flatMap(_.find(x => x.fromLayer == layer && x.from == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedClient(tl,t))
            } ++
            mappings.values.flatMap(_.find(x => x.toLayer == layer && x.to == named.eResource().getURIFragment(named))).toSeq.flatMap {
              case Mapping(fl,f,tl,t,s,o) =>
                Seq(ModifiedSupplier(fl,f))
            } :+ Modified(layer, named.eResource().getURIFragment(named))
          case other => Seq.empty
        }
      case other => Seq.empty
    }.toSet
    println(layer + ": " + issues.get(layer))
  }

  def proven(layer: String, constr: String) = () // Semantics.proven(layer,constr)

  def entities = layerObjects

  def evaluateNL(layer: Resource) = {
    val out = output.task(s"Evaluating NL layer '${layer.getURI}'")
    val reqs = layer.getAllContents.asScala.collect {
      case r: org.eclipse.papyrus.sysml.requirements.Requirement =>
        r
    }
    layerObjects += layer.getURI.toString -> reqs.map { r =>
      val pos = positions(layer.getURI.toString)(layer.getURIFragment(r.getBase_Class))
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

  def load(output: TaskOutput): (URI => Resource) = (uri: URI) => {
    output.info(s"loading $uri")
    val file = new File(uri.toFileString)
    val graphURI = URI.createURI(s"graph://${uri.lastSegment()}") // FIXME
    println(graphURI)
    store.graphDb.transaction {
      Option(store.graphDb.findNode(Labels.Resource,"originalURI",uri.toString)).fold {
        // resource not existent before
        val resource = resourceSet.createResource(graphURI).asInstanceOf[GraphResource]
        val positions =
          if (uri.fileExtension() == "cpp") ClangASTParser.parse(uri.toFileString,resource)
          else de.dfki.cps.specific.SysML.load(file,resource,includeProfileApplcations = false)
        this.positions(uri.toString) = positions.map {
          case (o,p) => resource.getURIFragment(o) -> p
        }
        this.mappings(uri.toString) = getMappings(resource)
        resource.save(new java.util.HashMap)
        resource.getContents.asScala.collectFirst {
          case m: uml.Model => m.getName
        }.foreach(name => resource.root.foreach(_.setProperty("model",name)))
        resource.root.foreach(_.setProperty("originalURI",uri.toString))
        resource.root.foreach(_.setProperty("mode",if (uri.fileExtension() == "cpp") "text/x-c++src" else "sysml"))
        resource.root.foreach(_.setProperty("lastModified",file.lastModified()))
        resource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
        layerObjects += uri.toString -> sysml.SysML.getEntities(resource,positions.map {
          case (o,p) => resource.getURIFragment(o) -> p
        }.lift)
        resource
      } { node =>
        if (node.getProperties("lastModified") == file.lastModified()) {
          // wasn't modified
          if (!this.positions.isDefinedAt(uri.toString)) {
            val tempRes = resourceSet.createResource(uri.appendFileExtension("ecore"))
            val positions =
              if (uri.fileExtension() == "cpp") ClangASTParser.parse(uri.toFileString,tempRes)
              else de.dfki.cps.specific.SysML.load(file,tempRes,includeProfileApplcations = false)

            issues(uri.toString) = Set.empty
            this.positions(uri.toString) = positions.map {
              case (o,p) => tempRes.getURIFragment(o) -> p
            }
            this.mappings(uri.toString) = getMappings(tempRes)
            layerObjects += uri.toString -> sysml.SysML.getEntities(tempRes,positions.map {
              case (o,p) => tempRes.getURIFragment(o) -> p
            }.lift)
          }
          resourceSet.getResource(graphURI, true).asInstanceOf[GraphResource]
        } else {
          // was potentially modified
          val oldResource =  resourceSet.createResource(graphURI).asInstanceOf[GraphResource]
          oldResource.load(new java.util.HashMap)
          val newResource = resourceSet.createResource(uri.appendFileExtension("ecore"))
          val positions =
            if (uri.fileExtension() == "cpp") ClangASTParser.parse(uri.toFileString,newResource)
            else de.dfki.cps.specific.SysML.load(file,newResource,includeProfileApplcations = false)
          this.positions(uri.toString) = positions.map {
            case (o,p) => newResource.getURIFragment(o) -> p
          }
          val diff = de.dfki.cps.secore.stools.getSTool("specific").sdiff(new SResource(oldResource), new SResource(newResource))
          updateIssues(uri.toString, diff)
          this.mappings(uri.toString) = getMappings(newResource)
          layerObjects += uri.toString -> sysml.SysML.getEntities(newResource,positions.map {
            case (o,p) => newResource.getURIFragment(o) -> p
          }.lift)
          de.dfki.cps.egraph.stools.Diff.applyDiff(oldResource,diff)
          oldResource.unload()
          oldResource.load(new java.util.HashMap)
          newResource.getContents.asScala.collectFirst {
            case m: uml.Model => m.getName
          }.foreach(name => oldResource.root.foreach(_.setProperty("model",name)))
          oldResource.root.foreach(_.setProperty("lastModified",file.lastModified()))
          oldResource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
          oldResource
        }
      }
    }.get
  }

  def layers = store.graphDb.transaction {
    Specs(store.graphDb.findNodes(Labels.Resource).asScala.filter(_.hasProperty("model")).map { node =>
      Spec(
        node.getProperty("model").asInstanceOf[String],
        node.getProperty("originalURI").asInstanceOf[String],
        node.getProperty("content").asInstanceOf[String],
        node.getProperty("mode").asInstanceOf[String]
      )
    }.toList)
  }.get

  def commit(): Unit = {
    val out = output.task("Updating")
    val uri = URI.createFileURI("example/index.sysml")
    out.info(s"loading project $uri")
    val graphURI = URI.createURI("graph://index")
    val file = new File(uri.toFileString)
    store.graphDb.transaction {
      Option(store.graphDb.findNode(Labels.Resource,"originalURI",uri.toString)).fold {
        // resource not existent before
        val resource = resourceSet.createResource(graphURI).asInstanceOf[GraphResource]
        val positions = de.dfki.cps.specific.SysML.loadProject(uri,resource,load(out))
        this.positions(uri.toString) = positions.map {
          case (o,p) => resource.getURIFragment(o) -> p
        }
        this.mappings(uri.toString) = getMappings(resource)
        resource.save(new java.util.HashMap)
        resource.root.foreach(_.setProperty("originalURI",uri.toString))
        resource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
        layerObjects += uri.toString -> sysml.SysML.getEntities(resource,positions.map {
          case (o,p) => resource.getURIFragment(o) -> p
        }.lift)
        resource
      } { node =>
        // was potentially modified
        println("diff project")
        val oldResource =  resourceSet.getResource(graphURI,true).asInstanceOf[GraphResource]
        oldResource.load(new java.util.HashMap)
        val newResource = resourceSet.createResource(uri.appendFileExtension("ecore"))
        val positions = de.dfki.cps.specific.SysML.loadProject(uri,newResource,load(out))
        this.positions(uri.toString) = positions.map {
          case (o,p) => newResource.getURIFragment(o) -> p
        }
        this.mappings(uri.toString) = getMappings(newResource)
        val diff = de.dfki.cps.secore.stools.getSTool("specific").sdiff(new SResource(oldResource), new SResource(newResource))
        de.dfki.cps.egraph.stools.Diff.applyDiff(oldResource,diff)
        oldResource.unload()
        oldResource.load(new java.util.HashMap)
        oldResource.root.foreach(_.setProperty("lastModified",file.lastModified()))
        oldResource.root.foreach(_.setProperty("content",Source.fromFile(file).mkString))
        oldResource
      }
    }.get
    out.taskDone()
  }

  def normalizeURI(uri: URI): URI = { // FIXME: Quick Hack
    if (uri.scheme() == "graph")
      URI.createFileURI(s"example/${uri.host()}")
    else if (uri.fileExtension()== "ecore")
      uri.trimFileExtension()
    else uri
  }

  def createMapping(fromLayer: String, from: LayerObject, toLayer: String, to: LayerObject) = {
    val fromFileURI = URI.createFileURI(fromLayer)
    val fromURI = URI.createURI(s"graph://${fromFileURI.lastSegment()}")

    val toFileURI = URI.createFileURI(toLayer)
    val toURI = URI.createURI(s"graph://${fromFileURI.lastSegment()}")

    val f = resourceSet.getResource(fromURI,true)
    val t = resourceSet.getResource(toURI,true)

    val fromObj = f.getEObject(from.path).asInstanceOf[uml.NamedElement]
    val toObj = t.getEObject(to.path).asInstanceOf[uml.NamedElement]

    println(fromObj,toObj)
  }

  def removeMappings(layer: String, to: LayerObject) = ()
    //Semantics.removeMappings(layer, to)

  def ignoreModel(layer: String, model: LayerObject, reason: String) = {
    //Semantics.ignoreModel(layer, model, reason)
  }

  def getMappings(layer: Resource): Set[Mapping] = {
    val mappings = layer.getAllContents.asScala.collect {
      case s: uml.StateMachine =>
        s.getRegions.asScala
          .flatMap(_.getTransitions.asScala)
          .flatMap(_.getTriggers.asScala.map(_.getEvent))
          .collect {
            case e: uml.CallEvent if e.getOperation != null =>
              val op = e.getOperation
              Mapping(
                normalizeURI(op.eResource().getURI).toString,
                op.eResource().getURIFragment(op),
                normalizeURI(s.eResource().getURI).toString,
                s.eResource().getURIFragment(s),
                "uses",
                None
              )
          }
      case r: uml.Realization =>
        val s = r.getSuppliers.get(0)
        val cs = r.getClients.asScala
        cs.map { c =>
          Mapping(
            normalizeURI(c.eResource().getURI).toString,
            c.eResource().getURIFragment(c),
            normalizeURI(s.eResource().getURI).toString,
            s.eResource().getURIFragment(s),
            "realization",
            Option(r.getMapping).map(_.getBodies.asScala.mkString("\n"))
          )
        }
      case s: requirements.Satisfy =>
        val abstr = s getBase_Abstraction()
        println(abstr)
        val su = abstr.getSuppliers.get(0) match {
          case r: requirements.Requirement => r.getBase_Class
          case other => other
        }
        val cs = abstr.getClients.asScala
        cs.map { c =>
          println(c.eResource().getURIFragment(c))
          println(su.eResource().getURIFragment(su))
          Mapping(
            normalizeURI(c.eResource().getURI).toString,
            c.eResource().getURIFragment(c),
            normalizeURI(su.eResource().getURI).toString,
            su.eResource().getURIFragment(su),
            "satisfy",
            None
          )
        }
    }.flatten
    mappings.toSet
  }
}