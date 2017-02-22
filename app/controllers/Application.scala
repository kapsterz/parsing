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
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json._

import scala.concurrent.duration._
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

import scala.concurrent.{Await, ExecutionContext, Future}
import reactivemongo.bson.BSONDocument
import reactivemongo.api.commands.WriteResult
import reactivemongo.api.collections.bson.BSONCollection

import scala.util.{Failure, Success}
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
import java.lang.String._

import play.api.Logger
import play.twirl.api.Html

import scala.collection.GenTraversable
import scala.collection.immutable.IndexedSeq

class Application @Inject()(ws: WSClient)
                           (implicit executionContext: ExecutionContext,
                            implicit val reactiveMongoApi: ReactiveMongoApi)
  extends Controller
    with MongoController
    with ReactiveMongoComponents {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  private val pattern = new Regex("([0-9]+\\.){3}[0-9]+:+([1-9]\\d{0,5})")
  private val collection = reactiveMongoApi.database.map(_.collection[BSONCollection]("test"))

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


  private def pom(l: String, reqs: String = "alive+proxy", num: Int = 0) = l match {
    case "Google" => s"https://google.com/search?q=$reqs&start=$num"
    case "Bing" => s"https://www.bing.com/search?q=$reqs&first=$num"
    case s => s
  }

  private def reqAttr(urr: String): WSRequest = ws
    .url(urr)
    .withHeaders("User-Agent" -> USER_AGENT)
    .withFollowRedirects(true)

  private def goInToUrl(url: String) = reqAttr(url).get()

  private def Url(v: String, reqs: String = "alive+proxy", num: Int = 0) = goInToUrl(pom(v, reqs, num))

  def request(userData: UserData = UserData()): Seq[Future[WSResponse]] =
    userData match {
      case UserData(f, _, _, d, "2", _) => f match {
        case "Combine" =>
          Seq(Url("Google", num = d * 10), Url("Bing", num = d))
        case _ =>
          Seq(Url(f))
      }
      case UserData(_, _, l, d, "1", o) => Seq(Url(o, l, num = d))
      case UserData(_,_,_,_,_,_) => throw sys.error( "Error in line 93, Bad request")
    }


  private def proxyCheck(proxys: List[String]): Future[List[String]] = {
    def acc(pr: List[String], doneProxy: List[String]): Future[List[String]] = {
      pr.isEmpty match {
        case false =>
          val n: Int = pr.head.indexOf(":")
          val defProxy: WSProxyServer = DefaultWSProxyServer(protocol = Some("http"),
            host = pr.head.substring(0, n),
            port = pr.head.substring(n + 1, pr.head.length).toInt)
          val check = reqAttr(pom("http://www.google.com.ua/search?q=аауоа"))
            .withProxyServer(defProxy)
            .withRequestTimeout(Duration(30, SECONDS))
            .get()
          check.flatMap {
            case f if f.status == 200 => acc(pr.tail, pr.head +: doneProxy)
            case _ => acc(pr.tail, doneProxy)
          }.recoverWith {
            case ex =>
              //              Logger.warn(ex.getStackTrace.mkString("\n"))
              acc(pr.tail, doneProxy)
          }
        case true => Future.successful(doneProxy)
      }
    }

    acc(proxys, List.empty)
  }


  private def reqq(requestValue: String = "Google"): PartialFunction[WSResponse, Future[List[String]]] = {
    case f if f.body.nonEmpty =>
      val doc: Document = Jsoup.parse(f.body)

      def acc(i: Int,
              docz: Document,
              s: List[String]): Future[List[String]] = {
        i match {
          case depth if depth > 1 =>
            val docx = requestValue match {
              case "Google" =>
                val s = goInToUrl("https://google.com" + docz.select("h3.r a").first().attr("href"))
                docz.select("h3.r a").first().remove()
                s
              case "Bing" =>
                val s = goInToUrl(docz.select("h2 a").first().attr("href"))
                println(docz.select("h2 a").first().attr("href"))
                docz.select("h2 a").first().remove()
                s
            }
            docx.map(pattern findAllIn _.body toList).flatMap {
              case elements if elements.nonEmpty =>
                acc(depth - 1, docz, s ++: elements)
              case _ =>
                acc(depth - 1, docz, s)
            }
          case _ => Future.successful(s)
        }
      }
      val res = acc(10, doc, List.empty)
      res.map(f => {
        splitParProxy(f, f.length).map{ l =>

        if (l.nonEmpty) {
          val docc = Json.obj(
            "proxy" -> l.map(z =>
              JsString(z)
            )
          )

          val writeRes = collection.map(_.insert(docc))
          writeRes onComplete {
            case Failure(e) => println("Error adding elements in DB: " + e)
            case Success(writeResult) => println("Add elements in DB success: " + writeResult)
          }

        } else {
          println("Error: nothing to add in DB :(")
        }
      }
      }
      )
      res

    case _ =>
      println("Error: Response body empty!")
      Future.successful(List.empty)
  }

  def getFromDb: Future[List[String]] = {
    val god = collection.map(
      f =>
        f.find(
          BSONDocument("proxy" ->
            BSONDocument(
              "$gt" -> "$gt"
            )
          )
        ).cursor[JsObject]()
          .collect[List]()
          .map(s =>
            s.last.value.last._2.asOpt[List[String]].get
          )
    )
    god flatMap {
      f =>
        f map {
          d => d
        }
    }
  }

  private def splitParProxy(list: List[String], int: Int): Future[List[String]] = {
    val res = for (ints <- 1 to int) yield proxyCheck(
      list.drop(list.length * (ints - 1) / int)
        .dropRight(list.length - list.length * ints / int))
      .map { f => f }
    Future.sequence(res).map(_.flatten.toList)
  }

  private def kekz(userData: UserData = UserData()): Html = {
    val x: Future[List[String]] = userData.pasreFromURL match {
      case "2" =>
        request().head.map(reqq(userData.requestValue))
        if (request().tail.nonEmpty)  request().tail.head.flatMap(reqq(userData.requestValue))
        else Future.successful(List("Nothing found"))
      case "1" => request().head.map {
        w =>
          List(w.body.contains(userData.requestAnother).toString)
      }
    }
    Await(views.html.index(x),)


  }

  def index = Action {
    Ok(kekz())
  }

  def test = Action {
    Ok(views.html.test(userForm))
  }

  def userPost: Action[UserData] = Action(parse.form(userForm)) {
    implicit request =>
      val userData = request.body
      val newUserData = this.request(UserData(userData.requestValue, userData.requestType, userData.requestAnother, userData.numberOfPages, userData.pasreFromURL, userData.urlToParse))
      userData.pasreFromURL match {
        case "2" =>
          newUserData.head.map(reqq(userData.requestValue))
          if (newUserData.tail.nonEmpty) newUserData.tail.head.map(reqq(userData.requestValue))
        case "1" => newUserData.head.map {
          w =>
            println(w.body.contains(userData.requestAnother))
        }
      }
      Await.result(getFromDb.map(d => {
        splitParProxy(d, d.length).foreach(println(_))
      }), 100.second)
      Redirect(routes.Application.test())
  }
}








