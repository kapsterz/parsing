
import actors._
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport
/**
  * Created by wegod on 27.02.2017.
  */
class Module extends AbstractModule with AkkaGuiceSupport {
  def configure = {
    bindActor[MainActor]("actorMainActor")
    bindActor[Requests]("actorRequests")
  }
}
