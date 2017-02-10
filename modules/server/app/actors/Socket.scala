package actors

import akka.actor.{Actor, ActorRef}
import specific._
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
    case MSG(CommitAll) => {
      CM.commit()
      send(MappingsList(CM.mappings.values.flatten.toSet))
      CM.listIssues()
    }
    case MSG(Commit) => {
      CM.commit()
      send(InitSpecs(CM.layers))
      send(MappingsList(CM.mappings.values.flatten.toSet))
      CM.listIssues()
    }
    case MSG(IgnoreModel(layer,model,reason)) => {
      CM.ignoreModel(layer,model,reason)
      send(MappingsList(CM.mappings.values.flatten.toSet))
      CM.listIssues()
    }
    case MSG(AddMapping(fromLayer,from,toLayer,to)) => {
      CM.createMapping(fromLayer,from,toLayer,to)
      send(MappingsList(CM.mappings.values.flatten.toSet))
      CM.listIssues()
    }
    case MSG(RemoveMappings(layer,to)) => {
      CM.removeMappings(layer, to)
      send(MappingsList(CM.mappings.values.flatten.toSet))
      CM.listIssues()
    }
    case MSG(Proven(layer,constr)) =>
      CM.proven(layer,constr)
      CM.listIssues()
    case MSG(Evaluate(Spec(name,uri,content,mode))) =>
      /*val layer = CM.project.nl.find(_.name == name).get
      layer.source = content
      CM.evaluateNL(layer)
      CM.listIssues()*/
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
    CM.commit()
    send(InitSpecs(CM.layers))
    CM.entities.foreach {
      case (name, entities) =>
        send(Entities(name,entities))
    }
    send(MappingsList(CM.mappings.values.flatten.toSet))
    CM.listIssues()
  }

  override def postStop(): Unit = {
    CM.output.remove(output)
  }
}

object MSG {
  def unapply(msg: String): Option[Message] = {
    Try(read[Message](msg)).toOption
  }
}
