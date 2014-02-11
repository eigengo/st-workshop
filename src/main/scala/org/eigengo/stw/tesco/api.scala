package org.eigengo.stw.tesco

import spray.json.DefaultJsonProtocol
import spray.http.Uri
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

object TescoApiActor {

  case object Login
  case class Search(query: String)

  case object NotLoggedIn
  case class LoggedIn(sessionId: String) extends AnyVal

}

class TescoApiActor(uri: Uri, credentials: Credentials) extends Actor with DefaultJsonProtocol with SprayJsonSupport {
  this: Authentication =>
  import TescoApiActor._
  import commands._
  import akka.pattern.pipe
  import spray.client.pipelining._
  import context.dispatcher

  implicit val ProductFormat = jsonFormat3(Product)
  implicit val ProductSearchFormat = jsonFormat2(ProductSearchResponse)

  val io = IO(Http)(context.system)
  val productSearch = sendReceive ~> unmarshal[ProductSearchResponse]
  doLogin()

  def doLogin() = login(uri, credentials).map {
    case Left(error) => context.become(receive); NotLoggedIn
    case Right(sessionId) => context.become(loggedIn(sessionId)); LoggedIn(sessionId)
  }

  def loggedIn(sessionId: String): Receive = {
    def command(command: String, queryParameters: (String, String)*) = {
      val params = Map("command" -> command, "sessionkey" -> sessionId) ++ queryParameters
      val x = uri.withQuery(params)
      println(x)
      Get(x)
    }

    {
      case Search(query) =>
        productSearch(command(ProductSearch, ("searchtext", query))) pipeTo sender
    }
  }

  def receive: Receive = {
    case Login => sender ! doLogin
    case _ => sender ! NotLoggedIn
  }

}