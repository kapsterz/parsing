package actors

import javax.inject._

import akka.actor._
import models.forms.{UrlModel, UserData}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.libs.ws._
import com.google.inject.name.Named
import play.api.Configuration

import scala.concurrent.Future


class Requests @Inject()(@Named("actorMainActor") actorMainActor: ActorRef,
                         configuration: Configuration) extends Actor {

  private val pattern = """((\d{1,3}\.){3}\d{1,3}:([1-9]\d{0,5}))""".r
  private var count: Int = 1

  override def receive: Receive = {
    case userData: UserData => parsFromUrls(userData)
    case responseFromSite: WSResponse =>
      println("Found response:" + responseFromSite + "count" + count)
      count += 1
      proxiesFinder(responseFromSite.body)
    case (responseFromSearcher: WSResponse, userData: UserData) => successSearcherRequest(responseFromSearcher, userData)
  }

  private def urlMaker(urlModel: UrlModel) = urlModel.reqValue match {
    case "Google" =>
      s"http://google.com/search?q=${urlModel.request}&start=${urlModel.pageNum}&filter=0"
    case "Bing" =>
      s"https://www.bing.com/search?q=${urlModel.request}&first=${urlModel.pageNum}"
    case s => s
  }


  def request(userData: UserData = UserData(), urlFunc: UrlModel => String = urlMaker): Unit =
    userData match {
      case UserData(requestValue, _, requestAnother, numberOfPages, pasreFromURL, _) if pasreFromURL == "2" =>
        actorMainActor ! (urlFunc(UrlModel(requestValue, requestAnother, (numberOfPages - 1) * 10)) -> userData)
      case UserData(_, _, requestAnother, _, pasreFromURL, urlToParse) if pasreFromURL == "1" =>
        actorMainActor ! (urlMaker(UrlModel(urlToParse, requestAnother, 0)) -> userData)
      case UserData(_, _, _, _, _, _) => throw sys.error("Error in line 42, Bad request")
    }


  private def proxiesFinder(docWithProxies: String): Unit = {
    pattern.findAllMatchIn(docWithProxies).foreach { proxy =>
      println(proxy)
      actorMainActor ! models.forms.Proxy(proxy = proxy.group(1))
    }
  }

  private def successSearcherRequest(responseFromSearcher: WSResponse, userData: UserData): Unit = {
    val docFromSearcher: Document = Jsoup.parse(responseFromSearcher.body)
    for (_ <- 1 to 10) {
      docFromSearcher match {
        case q if !q.select("h3.r a").isEmpty =>
          actorMainActor ! q.select("h3.r a").first().attr("href")
          q.select("h3.r a").first().remove()
        case q if !q.select("h2 a").isEmpty =>
          actorMainActor ! q.select("h2 a").first().attr("href")
          q.select("h2 a").first().remove()
        case ex => Future.failed(sys.error("Error: in request < 10 URLs. This request: " + ex))
      }
    }
  }

  private def parsFromUrls(userData: UserData): Unit = {
    for (currentPage <- 1 to userData.numberOfPages) yield
      request(UserData(userData.requestValue,
        userData.requestType,
        userData.requestAnother,
        currentPage))
  }

}

