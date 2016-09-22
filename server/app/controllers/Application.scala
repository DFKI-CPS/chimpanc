package controllers

import play.api.mvc._
import akka.actor.Props
import play.api.libs.json.JsValue
import play.api.Play.current

object Application extends Controller {
  System.setProperty("wordnet.database.dir", "./change-management/wordnet/dict");

  def index() = Action { implicit request =>
    Ok(views.html.index())
  }
  
  def session() = WebSocket.acceptWithActor[String,String] { implicit request =>
    out => Props(classOf[actors.Socket], out)
  }
}
