package org.eigengo.stw.tesco

import spray.json.DefaultJsonProtocol
import spray.http.{ContentTypes, HttpEntity, HttpResponse, Uri}
import akka.actor.Actor
import spray.can.Http
import akka.io.IO
import spray.httpx.SprayJsonSupport
import scala.concurrent.Future

case class Credentials(email: String, password: String, developerKey: String, applicationKey: String)

trait Authentication {
  type SessionId = String

  def login(uri: Uri, credentials: Credentials): Future[Either[String, SessionId]]
}

/**
 * Companion object for the API actor
 */
object TescoApiActor {

  case object Login
  case class Search(query: String)

  case object NotLoggedIn
  case class LoggedIn(sessionId: String) extends AnyVal

}

/**
 * Uses the Tesco API to perform product searches
 *
 * @param uri the Tesco API base URI
 * @param credentials the credentials to use to log in
 */
class TescoApiActor(uri: Uri, credentials: Credentials) extends Actor with DefaultJsonProtocol with SprayJsonSupport {
  this: Authentication =>
  import TescoApiActor._
  import commands._
  import akka.pattern.pipe
  import spray.client.pipelining._
  import context.dispatcher

  // unmarshalling support
  implicit val ProductFormat = jsonFormat3(Product)
  implicit val ProductSearchFormat = jsonFormat2(ProductSearchResponse)

  // low-level IO
  val io = IO(Http)(context.system)
  val productSearch = sendReceive ~> forceApplicationJson ~> unmarshal[ProductSearchResponse]

  // login
  doLogin()

  def forceApplicationJson(response: HttpResponse): HttpResponse = response.mapEntity(entity => HttpEntity(ContentTypes.`application/json`, entity.data))

  def doLogin() = login(uri, credentials).map {
    case Left(error) => context.become(receive); NotLoggedIn
    case Right(sessionId) => context.become(loggedIn(sessionId)); LoggedIn(sessionId)
  }

  /**
   * Implements behaviour when logged in.
   *
   * @param sessionId the session id
   * @return the actor behaviour
   */
  def loggedIn(sessionId: String): Receive = {
    def command(command: String, queryParameters: (String, String)*) = {
      val params = Map("command" -> command, "sessionkey" -> sessionId) ++ queryParameters
      Get(uri.withQuery(params))
    }

    {
      case Search(query) =>
        productSearch(command(ProductSearch, ("searchtext", query))) pipeTo sender
    }
  }

  /**
   * Implements behaviour when not logged in.
   *
   * @return the actor behaviour
   */
  def receive: Receive = {
    case Login => sender ! doLogin()
    case _     => sender ! NotLoggedIn
  }

}