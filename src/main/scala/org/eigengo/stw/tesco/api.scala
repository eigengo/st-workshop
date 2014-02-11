package org.eigengo.stw.tesco

import spray.json._
import spray.http._
import akka.actor.Actor
import spray.httpx.SprayJsonSupport
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http.HttpResponse

/**
 * Login credentials. See [Tesco API](https://secure.techfortesco.com/tescoapiweb/secretlogin.aspx) for more details.
 *
 * @param email the email address
 * @param password the password
 * @param developerKey the developer key
 * @param applicationKey the application key
 */
case class Credentials(email: String, password: String, developerKey: String, applicationKey: String)

trait Authentication {
  type SessionId = String
  type ErrorMessage = String

  /**
   * Starts the login process for the Tesco API at the given ``uri``, using the supplied ``credentials``
   *
   * @param uri the Tesco API uri
   * @param credentials the credentials to use
   * @return future of either some failure or valid sessionId
   */
  def login(uri: Uri, credentials: Credentials): Future[Either[ErrorMessage, SessionId]]
}

/**
 * ``Authentication`` implementation that contacts the real Tesco API to log you in
 */
trait RealApiAuthentication extends Authentication with ContentTypeDirectives {
  this: Actor =>
  import context.dispatcher
  lazy val login = sendReceive ~> forceApplicationJson ~> extractSessionId

  def extractSessionId(response: HttpResponse): Either[ErrorMessage, SessionId] = {
    val json = JsonParser(response.entity.asString(defaultCharset = HttpCharsets.`UTF-8`))
    val fields = json.asJsObject.fields
    (fields.get("StatusCode"), fields.get("SessionKey")) match {
      case (_, Some(JsString(sessionId))) => Right(sessionId)
      case (Some(statusCode), _)          => Left(s"Not logged in (code $statusCode).")
      case _                              => Left(s"Not logged in.")
    }
  }

  override def login(uri: Uri, credentials: Credentials): Future[Either[ErrorMessage, SessionId]] = {
    val parameters = Map("command" -> "LOGIN", "email" -> credentials.email, "password" -> credentials.password,
                         "developerkey" -> credentials.developerKey, "applicationkey" -> credentials.applicationKey)
    login(Get(uri.withQuery(parameters)))
  }
}

/**
 * Companion object for the API actor
 */
object TescoApiActor {

  /**
   * Performs the login again
   */
  case object Login

  /**
   * Performs product search; you may search for 'chocolate' or 'beer' or any full text expression
   *
   * @param query the query string
   * @param page the page to find
   */
  case class Search(query: String, page: Option[Int])

  /**
   * Lists all current offers
   * @param page the page to find
   */
  case class ListOffers(page: Option[Int])

  /**
   * Indicates that you have not logged in (successfully or yet)
   */
  case object NotLoggedIn

  /**
   * Response to the logged in command
   * @param sessionId the generated sessionId
   */
  case class LoggedIn(sessionId: String) extends AnyVal

}

trait ContentTypeDirectives {
  def forceApplicationJson(response: HttpResponse): HttpResponse = response.mapEntity(entity => HttpEntity(ContentTypes.`application/json`, entity.data))
}

/**
 * Uses the Tesco API to perform product searches
 *
 * @param uri the Tesco API base URI
 * @param credentials the credentials to use to log in
 */
class TescoApiActor(uri: Uri, credentials: Credentials) extends Actor with DefaultJsonProtocol with SprayJsonSupport with ContentTypeDirectives {
  this: Authentication =>
  import TescoApiActor._
  import commands._
  import akka.pattern.pipe
  import context.dispatcher

  // unmarshalling support
  implicit val ProductFormat = jsonFormat3(Product)
  implicit val ProductSearchFormat = jsonFormat2(ProductSearchResponse)

  // low-level IO
  val productSearch = sendReceive ~> forceApplicationJson ~> unmarshal[ProductSearchResponse]

  // login
  doLogin()

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
    def command(command: String, page: Option[Int], queryParameters: (String, String)*) = {
      val params = Map("command" -> command, "sessionkey" -> sessionId) ++ queryParameters ++ page.map(p => Map("page" -> p.toString)).getOrElse(Map())
      Get(uri.withQuery(params))
    }

    {
      case Search(query, page) => productSearch(command(ProductSearch, page, ("searchtext", query))) pipeTo sender
      case ListOffers(page)    => productSearch(command(ListProductOffers, page)) pipeTo sender
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