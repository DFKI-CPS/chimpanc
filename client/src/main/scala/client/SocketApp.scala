package client

import org.scalajs.dom._
import org.scalajs.dom.raw.WebSocket
import specific.Message
import scala.scalajs.js.JSApp
import Util._
import scala.util.matching.Regex

/**
 * Created by martin on 08.09.15.
 */
abstract class SocketApp[I,O](url: String)(deserialize: String => I, serialize: O => String) extends JSApp {
  private var socket: Option[WebSocket] = None

  def receive: PartialFunction[I,Unit]

  def send(msg: O) = socket.fold(
    sys.error(s"trying to send $msg while socket is closed")
  )(
  (s: WebSocket) => {
    console.log("[debug] send: ", msg.toString)
    s.send(serialize(msg))
  })

  def main(): Unit = whenReady {
    println("starting")
    val socket = new WebSocket(url)
    socket.on(Event.Socket.Open)    (e => socketOpened(socket))
    socket.on(Event.Socket.Message) (e => rawReceive(e.data.asInstanceOf[String]))
    socket.on(Event.Socket.Close)   (e => socketClosed())
  }

  def socketOpened(socket: WebSocket) {
    this.socket = Some(socket)
    console.log("socket open")
  }

  def rawReceive(msg: String) = {
    val it = deserialize(msg)
    receive(it)
  }

  def socketClosed(): Unit = {
    this.socket = None
    location.reload()
  }
}
