package controllers

import play.api.libs.ws._
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import models.forms._
import play.api.mvc._
import akka.actor._
import javax.inject._

import models.{ProxiesDAO, ReCheckDB}
import play.api.Configuration

import scala.concurrent.ExecutionContext

class Application @Inject()(@Named("actorRequests") actorRequests: ActorRef, ws: WSClient,
                            proxiesDAO: ProxiesDAO, configuration: Configuration, reCheckDB: ReCheckDB)
                           (implicit executionContext: ExecutionContext)
  extends Controller {

  private val userForm = Form(
    mapping(
      "requestValue" -> text,
      "requestType" -> text,
      "requestAnother" -> text,
      "numberOfPages" -> number,
      "pasreFromURL" -> text,
      "urlToParse" -> text
    )(UserData.apply)(UserData.unapply)
  )

  def index(proxySet: List[String] = List.empty): Action[AnyContent] =
    proxySet match {
      case d if d.nonEmpty => Action {
        Ok(views.html.index(proxySet))
      }
      case _ =>
        Action.async {
          actorRequests ! UserData()
          proxiesDAO.find().mapTo[List[Proxy]].map { message =>
            Ok(views.html.index(message.map(_.proxy)))
          }
        }
    }

  def test = Action {
    Ok(views.html.test(userForm))
  }

  def checkDB: Action[AnyContent] = Action.async {
    reCheckDB.deleteSame().map(
      _ =>
      Ok(views.html.test(userForm))
    )
  }

  def userPost: Action[UserData] = Action.async(parse.form(userForm)) {
    implicit request =>
      actorRequests ! request.body
      proxiesDAO.find().map { message =>
        Redirect(routes.Application.index(message.map(_.proxy)))
      }
  }
}








