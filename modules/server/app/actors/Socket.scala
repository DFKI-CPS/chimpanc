package actors

import _root_.specific.graph.Semantics
import akka.actor.{Actor, ActorRef}
import specific._
import specific.project.Layer
import upickle.default._

import scala.util.Try

/**
 * @author martin
 */
class Socket(out: ActorRef) extends Actor {
  import _root_.specific.{ChangeManagement => CM}
  
  def send(msg: Message) {
    out ! write(msg)
  }
  
  def receive = {
    case MSG(Commit(NL(name,content))) => {
      val layer = CM.project.nl.find(_.name == name).get
      layer.source = content
      CM.commit(name)
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(Commit(SysML(name,content))) => {
      val layer = CM.project.fsls.collectFirst {
        case x: Layer.SysML if x.name == name => x
      }.get
      layer.source = content
      CM.commit(name)
      CM.autoMap()
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(Commit(FSL(name,emf,ocl))) => {
      val layer = CM.project.fsls.collectFirst {
        case x: Layer.FSL if x.name == name => x
      }.get
      layer.emfSource = emf
      layer.oclSource = ocl
      CM.commit(name)
      CM.autoMap()
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(Commit(ESL(name,content))) => {
      val layer = CM.project.esl.find(_.name == name).get
      layer.source = content
      CM.commit(name)
      CM.autoMap()
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(IgnoreModel(layer,model,reason)) => {
      CM.ignoreModel(layer,model,reason)
      CM.autoMap()
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(AddMapping(fromLayer,from,toLayer,to)) => {
      CM.createMapping(fromLayer,from,toLayer,to)
      CM.autoMap()
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(RemoveMappings(layer,to)) => {
      CM.removeMappings(layer, to)
      CM.listMappings()
      CM.listIssues()
    }
    case MSG(Proven(layer,constr)) =>
      CM.proven(layer,constr)
      CM.listIssues()
    case MSG(Evaluate(NL(name,content))) =>
      val layer = CM.project.nl.find(_.name == name).get
      layer.source = content
      CM.evaluateNL(layer)
      CM.listIssues()
  }

  val output = new Output {
    override def task(name: String): TaskOutput = {
      send(StartTask(name))
      new TaskOutput {
        override def warn(msg: Any): Unit =
          send(Warn(name, msg.toString))

        override def error(message: String, error: Throwable): Unit =
          send(Error(name, message, error.getMessage))

        override def progress(value: Int): Unit =
          send(Progress(name, value))

        override def debug(msg: Any): Unit =
          send(Debug(name, msg.toString))

        override def taskDone(): Unit =
          send(EndTask(name))

        override def taskDone(result: Message): Unit = {
          send(EndTask(name))
          send(result)
        }

        override def info(msg: Any): Unit =
          send(Info(name, msg.toString))
      }
    }
  }

  override def preStart(): Unit =  {
    CM.output.add(output)
    send(InitSpecs(CM.project.toSpecs))
    CM.initialize()
    CM.entities.foreach {
      case (name, entities) =>
        println(name + ": " + entities)
        send(Entities(name,entities))
    }
    CM.autoMap()
    CM.listMappings()
    CM.listIssues()
  }

  override def postStop(): Unit = {
    CM.output.remove(output)
  }
}

object MSG {
  def unapply(msg: String): Option[Message] = {
    println(msg)
    Try(read[Message](msg)).toOption
  }
}