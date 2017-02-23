package controllers

import org.jsoup._
import org.jsoup.nodes._
import com.google.inject.Inject
import play.api.libs.ws.WSClient
import scala.util.matching.Regex
import play.api.mvc._
import play.api.libs.ws._
import play.api.libs.json._
import scala.concurrent.duration._
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import akka.actor.{ActorSystem}
import akka.stream.ActorMaterializer
import models.UserData
import reactivemongo.play.json._
import scala.concurrent.{Await, ExecutionContext, Future}
import reactivemongo.bson.BSONDocument
import reactivemongo.api.collections.bson.BSONCollection
import scala.util.{Failure, Random, Success}
import play.modules.reactivemongo.{MongoController, ReactiveMongoApi, ReactiveMongoComponents}
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
  private val userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"
  private val r = Random
  //  private val userAgent = USER_AGENT
  private val urlToCheck = "https://www.google.com/search?q=proxy&first=0"

  private def pom(l: String, reqs: String, num: Int = 0) = l match {
    case "Google" =>
      s"http://google.com/search?q=$reqs&start=$num&filter=0"
    case "Bing" =>
      s"https://www.bing.com/search?q=$reqs&first=$num"
    case s => s
  }

  private def reqAttr(urr: String): WSRequest = ws
    .url(urr)
    .withHeaders("User-Agent" -> userAgent)
    .withFollowRedirects(true)

  private def goInToUrl(url: String) = reqAttr(url).get()

  private def goInToUrlWithProxy(url: String, proxy: String) = {
    val n: Int = proxy.indexOf(":")
    val defProxy: WSProxyServer = DefaultWSProxyServer(protocol = Some("http"),
      host = proxy.substring(0, n),
      port = proxy.substring(n + 1, proxy.length).toInt)
    reqAttr(url)
      .withProxyServer(defProxy)
      .get()
  }

  private def Url(v: String, reqs: String, num: Int) = goInToUrl(pom(v, reqs, num))

  private def urlWithProxy(v: String,
                           reqs: String,
                           num: Int = 0, proxy: String) = goInToUrlWithProxy(pom(v, reqs, num), proxy)

  def request(userData: UserData = UserData(), url: (String, String, Int) => Future[WSResponse] = Url): Seq[Future[WSResponse]] =
    userData match {
      case UserData(f, _, e, d, "2", _) => f match {
        case "Combine" =>
          Seq(url("Google", e, (d - 1) * 10), url("Bing", e, (d - 1) * 10))
        case _ =>
          Seq(url(f, e, (d - 1) * 10))
      }
      case UserData(_, _, l, _, "1", o) => Seq(Url(o, l, 0))
      case UserData(_, _, _, _, _, _) => throw sys.error("Error in line 93, Bad request")
    }

  private def proxyCheck(proxys: Set[String]): Future[Set[String]] = {
    def acc(pr: Set[String], doneProxy: Set[String]): Future[Set[String]] = {
      pr.isEmpty match {
        case false =>
          val n: Int = pr.head.indexOf(":")
          val defProxy: WSProxyServer = DefaultWSProxyServer(protocol = Some("https"),
            host = pr.head.substring(0, n),
            port = pr.head.substring(n + 1, pr.head.length).toInt)
          val check = reqAttr(urlToCheck)
            .withProxyServer(defProxy)
            .withRequestTimeout(Duration(30, SECONDS))
            .get()
          check.flatMap {
            case f if f.status == 200 => acc(pr.tail, doneProxy + pr.head )
            case _ => acc(pr.tail, doneProxy)
          }.recoverWith {
            case ex =>
              //              Logger.warn(ex.getStackTrace.mkString("\n"))
              acc(pr.tail, doneProxy)
          }
        case true => Future.successful(doneProxy)
      }
    }

    acc(proxys, Set.empty)
  }

  private def reqq(userData: UserData): PartialFunction[WSResponse, Future[Set[String]]] = {
    case f if f.body.nonEmpty && (f.status == 200) =>
      val doc: Document = Jsoup.parse(f.body)

      def acc(i: Int,
              docz: Document,
              s: Set[String]): Future[Set[String]] = {
        i match {
          case depth if depth > 1 =>
            val docx = docz match {
              case q if !q.select("h3.r a").isEmpty =>
                val s = goInToUrl("https://google.com" + q.select("h3.r a").first().attr("href"))
                q.select("h3.r a").first().remove()
                s
              case q if !q.select("h2 a").isEmpty =>
                val s = goInToUrl(q.select("h2 a").first().attr("href"))
                q.select("h2 a").first().remove()
                s
              case ex => Future.failed(sys.error("Error: in request < 10 URLs. This request: " + ex))
            }
            docx.map(pattern findAllIn _.body toSet).flatMap {
              case elements if elements.nonEmpty =>
                acc(depth - 1, docz, s ++: elements)
              case _ =>
                acc(depth - 1, docz, s)
            }.recoverWith {
              case ex =>
                //                Logger.warn(ex.getStackTrace.mkString("\n"))
                acc(depth - 1, docz, s)
            }
          case _ => Future.successful(s)
        }
      }

      acc(10, doc, Set.empty)

    case f
      if f.status != 200
    =>

      getFromDb.flatMap { f =>
        val ran = r.nextInt(f.size)
        println("Google need a captcha! Trying with proxy: " + f.drop(ran).head)
        request(userData, urlWithProxy(_, _, _, f.drop(ran).head))
          .head
          .flatMap(reqq(userData)).recoverWith {
          case ex =>
            //                Logger.warn(ex.getStackTrace.mkString("\n"))
            val rand = r.nextInt(f.size)
            println("Proxy is bad! Google need a captcha! Trying with proxy: " + f.drop(rand).head)
            request(userData, urlWithProxy(_, _, _, f.drop(rand).head))
              .head
              .flatMap(reqq(userData))
        }
      }
    case _ =>
      println("Error: Response body empty!")
      Future.successful(Set.empty)
  }

  def writeInDb(l: Set[String]): Unit = {
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

  val getFromDb: Future[Set[String]] = {
    val god = collection.map(
      f =>
        f.find(
          BSONDocument("proxy" ->
            BSONDocument(
              "$gt" -> "$gt"
            )
          )
        ).cursor[JsObject]()
          .collect[Set]()
          .map(s =>
            s.last.value.last._2.asOpt[Set[String]].get
          )
    )
    god flatMap {
      f =>
        f map {
          d => d
        }
    }
  }

  private def splitParProxy(Set: Set[String], int: Int): Future[Set[String]] = {
    val res = for (ints <- 1 to int) yield proxyCheck(
      Set.drop(Set.size * (ints - 1) / int)
        .dropRight(Set.size - Set.size * ints / int))
      .map {
        f => f
      }
    Future.sequence(res).map(_.flatten.toSet)
  }

  private def parsFromUrls(userData: UserData): Future[Set[String]] = {
    val proxys = for (ints <- 1 to userData.numberOfPages) yield {
      val c = request(UserData(userData.requestValue, userData.requestType, userData.requestAnother, ints))
      c.nonEmpty match {
        case true if c.tail.nonEmpty =>
          Future.sequence(Seq(c.tail.head.flatMap(reqq(UserData("Google", userData.requestType, userData.requestAnother, ints))),
            c.head.flatMap(reqq(UserData("Bing", userData.requestType, userData.requestAnother, ints)))))
            .map(_.flatten.toSet)
        case true =>
          c.head.flatMap(reqq(userData))
        case false =>
          Future.successful(Set("Nothing found"))
      }
    }
    Future.sequence(proxys).map(f => f.toSet.flatten).flatMap(f => {
      val prox = splitParProxy(f, f.size)
      prox.flatMap { l =>
        writeInDb(l)
        prox
      }
    })
  }

  private def kekz(userData: UserData): Html = {
    val x: Future[Set[String]] = userData.pasreFromURL match {
      case "2" =>
        parsFromUrls(userData)
      case "1" =>
        request(userData).head.map {
          w =>
            Set(w.body.contains(userData.requestAnother).toString)
        }
    }
    Await.result(x.map(d => {
      views.html.index(d.toList)
    }), 500.second)

  }

  def index(proxySet: List[String] = List.empty): Action[AnyContent] = proxySet match {
    case d if d.nonEmpty => Action {
      Ok(views.html.index(proxySet))
    }
    case _ =>
      Action {
        Ok(kekz(UserData()))
      }
  }

  def test = Action {
    Ok(views.html.test(userForm))
  }

  def userPost: Action[UserData] = Action(parse.form(userForm)) {
    implicit request =>
      var now = System.nanoTime
      val userData: UserData = request.body
      val x: Future[Set[String]] = userData.pasreFromURL match {
        case "2" =>
          parsFromUrls(userData)
        case "1" =>
          this.request(userData).head.map {
            w =>
              Set(w.body.contains(userData.requestAnother).toString)
          }
      }
      Await.result(x.map(d => {
        println((System.nanoTime - now)/10^9)
        Redirect(routes.Application.index(d.toList))
      }), 500.second)
  }
}








