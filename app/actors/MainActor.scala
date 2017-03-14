package actors

import javax.inject._

import akka.actor._
import models.forms._
import play.api.libs.ws._
import com.google.inject.name.Named
import models.ProxiesDAO
import play.api.Configuration

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, SECONDS}

/**
  * Created by wegod on 24.02.2017.
  */


class MainActor @Inject()(proxiesDAO: ProxiesDAO, @Named("actorRequests") actorRequests: ActorRef,
                          configuration: Configuration, ws: WSClient)
  extends Actor {

  import context._

  private val urlToCheck = "https://google.com.ua"
  private val userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"

  override def receive: Receive = {
    case requestUrl: String =>
      println(requestUrl)
      parseProxiesFromSite(requestUrl)

    case (requestUrl: String, requestUserData: UserData, proxiesListFromDB: List[models.forms.Proxy]) =>
      parsFuncForResponse(requestUrl, requestUserData, proxiesListFromDB.head)

    case (pr: Proxy, f: String) if f == "CheckDB" =>
      val origin = sender()
      proxyChekerDB(pr).map { res =>
        println(pr, res)
        origin ! (pr -> res)
      }

    case pr: Proxy => proxyCheker(pr)

    case (requestUrl: String, requestUserData: UserData) =>
      println(requestUrl)
      parsFuncForResponse(requestUrl, requestUserData)
  }

  private def parsFuncForResponse(requestUrl: String, requestUserData: UserData,
                                  pr: models.forms.Proxy = models.forms.Proxy.empty): Unit = {
    if (Proxy.isEmpty(pr)) goInToUrl(requestUrl).map {
      case response if response.status == 200 && response.body.nonEmpty =>
        actorRequests ! (response, requestUserData)
      case response if response.status != 200 || response.body.isEmpty =>
        proxiesDAO.find(count = 1).map { proxiesListFromDB =>
          println("Google need a captcha! Trying with proxy: " + proxiesListFromDB)
          self ! (requestUrl, requestUserData, proxiesListFromDB)
        }
      case _ =>
        println("Error: Response body empty!")
    } else goInToUrlWithProxy(pr.proxy, requestUrl).map {
      case response if response.status == 200 && response.body.nonEmpty =>
        actorRequests ! (response, requestUserData)
      case response if response.status != 200 || response.body.isEmpty =>
        proxiesDAO.delete(pr)
        proxiesDAO.find(count = 1).map { proxiesListFromDB =>
          self ! (requestUrl, requestUserData, proxiesListFromDB)
        }
      case _ =>
        println("Error: Response body empty!")
    }
  }

  private def proxyCheker(pr: models.forms.Proxy): Future[Unit] = {
    goInToUrlWithProxy(pr.proxy).map {
      case response if response.status == 200 =>
        println("Found proxy: " + pr.proxy)
        proxiesDAO.insert(pr)
      case response =>
        println(response)
    }
  }

  private def proxyChekerDB(pr: models.forms.Proxy): Future[Boolean] = {
    goInToUrlWithProxy(pr.proxy).map {
      case response if response.status == 200 =>
        println("Found proxy: " + pr.proxy)
        true
      case response =>
        println(response)
        false
    }.recoverWith {
      case _ =>
        Future.successful(false)
    }
  }

  private def reqAttr(urr: String): WSRequest = ws.url(urr)
    .withHeaders("User-Agent" -> userAgent)
    .withFollowRedirects(true)

  private def goInToUrl(url: String): Future[WSResponse] = reqAttr(url).get()

  private def parseProxiesFromSite(url: String): Unit = goInToUrl(url).foreach(actorRequests ! _)

  private def goInToUrlWithProxy(proxy: String, urlToCheck: String = urlToCheck): Future[WSResponse] = {
    val n: Int = proxy.indexOf(":")
    val defProxy: WSProxyServer = DefaultWSProxyServer(protocol = Some("https"),
      host = proxy.substring(0, n),
      port = proxy.substring(n + 1, proxy.length).toInt)
    reqAttr(urlToCheck)
      .withProxyServer(defProxy)
      .withRequestTimeout(Duration(30, SECONDS))
      .get()
  }

}
