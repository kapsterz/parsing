package models

import javax.inject._

import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.modules.reactivemongo._
import reactivemongo.api.QueryOpts

import reactivemongo.play.json._
import reactivemongo.play.json.collection._
import reactivemongo.akkastream.{State, cursorProducer}
import models.forms._
//import reactivemongo.play.iteratees._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.parsing.json.JSONObject
import scala.util.{Failure, Success}


class ProxiesDAO @Inject()(val reactiveMongoApi: ReactiveMongoApi)
                          (implicit executionContext: ExecutionContext,
                           val m: Materializer)
  extends MongoController with ReactiveMongoComponents {

  private val collection = reactiveMongoApi.database.map(_.collection[JSONCollection]("test"))

  //  collection.map(_.indexesManager.create())
  def insert(p: forms.Proxy): Unit = {
    val writeRes = collection.map(_.insert(p))
    writeRes onComplete {
      case Failure(e) => println("Error adding elements in DB: " + e)
      case Success(writeResult) => println("Add elements in DB success: " + writeResult)
    }
  }

  def find(skipN: Int = 0, count: Int = 10): Future[List[Proxy]] = {
    collection.flatMap(
      f =>
        f.find(
          Json.obj("proxy" ->
            Json.obj("$exists" -> "true"))
        ).options(
          QueryOpts(skipN * 10, count))
          .cursor[Proxy]()
//                    .documentSource()
          //          .runWith(Sink.seq[JsObject])
          .collect[List]()
//          .map(s =>
//            s.map(
//              el => Proxy.jsObjectToProxy(el)
//            )
//          )
    )
    //    ).flatMap(
    //      _.map(
    //        _.map { el =>
    //          Proxy.jsObjectToProxy(el)
    //        }.toList
    //      )
    //    )
  }

  def findAllAsStream: Future[Source[Proxy, Future[State]]] = {
    collection.map(f => f.find(Json.obj()).cursor[Proxy]().documentSource())
  }

  def findProxy(pr: String): Future[List[Proxy]] ={
    collection.flatMap {
      f =>
        val s = f.find(Json.obj("proxy" -> pr ))
          .cursor[Proxy]()
          .collect[List]()
        println(s)
        s
    }
  }

  def delete(proxy: forms.Proxy): Future[Unit] = {
    println("Deleted: " + proxy)
    collection.map(_.remove(Json.obj("_id" -> proxy._id)))
  }

  //  override def receive: Receive = {
  //    case pr: String => insert(forms.Proxy(proxy = pr))
  //    case (skipN: Int, proxiesCount: Int) => find(skipN, proxiesCount).map(sender() ! _)
  //    case (requestUrl: String, requestUserData: UserData) => find(count = 1).map(sender() ! (requestUrl, requestUserData, _))
  //    case proxy: forms.Proxy => delete(proxy)
  //    case _ => println("WTF&")
  //  }

}
