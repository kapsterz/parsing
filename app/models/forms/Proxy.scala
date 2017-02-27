package models.forms

import reactivemongo.bson._
import play.api.libs.json._
import reactivemongo.play.json.BSONFormats.BSONObjectIDFormat
/**
  * Created by wegod on 23.02.2017.
  */

case class Proxy(_id: Option[BSONObjectID] = Some(BSONObjectID.generate()), proxy: String)

object Proxy {
  implicit val formatter: OFormat[Proxy] = Json.format[Proxy]
  def empty: Proxy = Proxy(None, "")
  def isEmpty(proxy: Proxy): Boolean =proxy match {
    case models.forms.Proxy(None, _) => true
    case models.forms.Proxy(_, "") => true
    case models.forms.Proxy(_, _) => false
  }
  def jsObjectToProxy(jsObject: JsObject):Proxy ={
    Proxy(Option(BSONObjectID(jsObject.value.head._2.toString().drop(9).dropRight(2))), jsObject.value.last._2.toString().drop(1).dropRight(1))
  }
}