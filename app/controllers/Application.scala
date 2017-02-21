package controllers

import play.api.mvc._
import org.jsoup._
import org.jsoup.nodes._
import java.io._

import com.google.inject.Inject
import org.jsoup.parser._
import play.api.libs.ws.WSClient

import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration._
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json._

import scala.concurrent.duration.Duration
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import models.UserData
import play.modules.reactivemongo._
import reactivemongo.api.ReadPreference
import reactivemongo.play.json._
import reactivemongo.play.json.collection._

import scala.concurrent.{ExecutionContext, Future}
import reactivemongo.bson.BSONDocument
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.collections.bson.BSONCollection

import scala.util.{Failure, Success}
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import java.lang.String._

import play.twirl.api.Html

class Application @Inject()(ws: WSClient)
                           (implicit executionContext: ExecutionContext,
                            implicit val reactiveMongoApi: ReactiveMongoApi)
  extends Controller
    with MongoController
    with ReactiveMongoComponents {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  private val pattern = new Regex("([0-9]+\\.){3}[0-9]+:+([1-9]\\d{0,5})")
  private val collection = reactiveMongoApi.database.map(_.collection[BSONCollection]("proxy"))

  private val userForm = Form(
    mapping(
      "requestValue" -> text,
      "requestType" -> text,
      "requestAnother" -> text
    )(UserData.apply)(UserData.unapply)
  )


  private def pom(l: String, reqs: String = "alive+proxy") = l match {
    case "Google" => s"https://google.com/search?q=$reqs&start=0"
    case "Yandex" => s"https://yandex.ua/search/?text=$reqs&p=0"
    case s => s
  }

  private def reqAttr(urr: String): WSRequest = ws
    .url(urr)
    .withHeaders("User-Agent" -> USER_AGENT)
    .withFollowRedirects(true)

  private def goInToUrl(url: String) = reqAttr(url).get()

  private def Url(v: String, reqs: String = "alive+proxy") = goInToUrl(pom(v, reqs))

  def request(userData: UserData = UserData()): Future[Seq[Future[WSResponse]]] =
    userData match {
      case UserData(f, "proxy", _) => f match {
        case "Combine" => Future {
          Seq(Url("Google"), Url("Yandex"))
        }
        case _ => Future {
          Seq(Url(f))
        }
      }
      case UserData(f, "another", l) => f match {
        case "Combine" => Future {
          Seq(Url("Google", l), Url("Yandex", l))
        }
        case _ => Future {
          Seq(Url(f, l))
        }
      }
    }

  private def proxyCheck(proxys: String): Future[Boolean] = {
    val n: Int = proxys.indexOf(":")
    val check = reqAttr(pom("Google"))
      .withProxyServer(DefaultWSProxyServer(protocol = Some("http"),
        host = proxys.substring(0, n),
        port = proxys.substring(n, proxys.length).toInt))
      .get()

    check map { f =>
      if (f.status == 200) true else false
    }
  }

  private val reqq: PartialFunction[WSResponse, Unit] = {
    case f =>
      val doc: Document = Jsoup.parse(f.body)

      def acc(i: Int,
              docz: Document,
              s: List[String]): Future[List[String]] = {
        if (i > 0) {
          val z = docz.select("h3.r a")
          val y = z.first()
          val x = y.absUrl("href")
          val docx = goInToUrl(x)
          println(docz)
          docx flatMap {
            f =>
              val docy = f.body
              if ((pattern findFirstIn docy).isDefined) {
                println("Curent pattern: " + (pattern findAllIn docy).mkString(", "))
                println("All of us: " + s)
              }
              docz.select("h3.r a").first().remove()
              acc(i - 1, docz, s ++: (pattern findAllIn docy).toList)
          }
        } else {
          Future.successful(s)
        }
      }

      val k: Future[List[String]] = {
        println("-----------That start!------------")
        acc(10, doc, List.empty)
      }

      k.map { f =>
        println("ACC: " + f)
        if (f.nonEmpty) {
          val docc = BSONDocument {
            f.map(d =>
              "proxy" -> d
            )
          }
          val writeRes = collection.map(_.insert(docc))
          writeRes onComplete {
            case Failure(e) => println("Error: " + e)
            case Success(writeResult) => println("Success: " + writeResult)
          }
        } else {
          println("Error: nothing to add in DB :(")
        }
      }
  }


  private def kekz(userData: UserData = UserData()): Html = {

    request().map(f => {
      println(f.head.map(reqq))
      println(f.tail.head.map(reqq))
    })

    views.html.index()
  }

  def index = Action {
    Ok(kekz())
  }

  def test = Action {
    Ok(views.html.test(userForm))
  }

  def userPost: Action[UserData] = Action(parse.form(userForm)) { implicit request =>
    val userData = request.body
    val newUser = this.request(UserData(userData.requestValue, userData.requestType, userData.requestAnother))
    println(request.body)
    Redirect(routes.Application.index())
  }
}








