package models

/**
  * Created by wegod on 26.02.2017.
  */

import javax.inject.Inject

import akka.actor._
import akka.pattern.ask
import akka.stream.Materializer
import akka.util.Timeout
import com.google.inject.name.Named
import models.forms._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class ReCheckDB @Inject()(@Named("actorMainActor") actorMainActor: ActorRef, proxiesDAO: ProxiesDAO)
                         (implicit executionContext: ExecutionContext, val materializer: Materializer) {
  implicit val timeout: Timeout = 60.seconds

  def deleteSame(): Future[Unit] = {
    proxiesDAO.findAllAsStream.map { f =>
      f.mapAsync(4) { res =>
        proxiesDAO.findProxy(res.proxy).map { r =>
          if (r.tail.nonEmpty) proxiesDAO.delete(res)
        }
      }
    }.flatMap(_.runForeach(_ => Unit))
      .map(_ => checkDB)
  }

  def checkDB: Future[Unit] = {
    proxiesDAO.findAllAsStream
      .map { f =>
        f.mapAsync(500) { g =>
          val s = (actorMainActor ? (g, "CheckDB"))
            .mapTo[(Proxy, Boolean)]
          s.map { res =>
            if (!res._2) proxiesDAO.delete(res._1)
          }
        }
      }
      .flatMap(_.runForeach(_ => Unit))
      .map { _ => println("Done")}
  }

}
