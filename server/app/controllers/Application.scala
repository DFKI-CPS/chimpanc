package controllers

import javax.inject._
import play.api.mvc._
import akka.actor._
import akka.stream.Materializer
import play.api.mvc.Results._

class Application @Inject()(implicit system: ActorSystem, materializer: Materializer, appProvider: Provider[play.api.Application]) {
  implicit lazy val application = appProvider.get()

  System.setProperty("wordnet.database.dir", "./change-management/wordnet/dict")

  def index() = Action { implicit request =>
    Ok(views.html.index())
  }

  def session() = WebSocket.acceptWithActor[String,String] { implicit request =>
    out => Props(classOf[actors.Socket], out)
  }
}
