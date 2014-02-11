package org.eigengo.stw.tesco

import akka.actor.{Props, ActorSystem}
import spray.http.Uri
import scala.concurrent.Future

object Main extends App {
  implicit val system = ActorSystem()

  val uri = Uri("http://www.techfortesco.com/groceryapi_b1/restservice.aspx")
  val credentials = Credentials("email@address.com", "pwd", "dev-key", "app-key")
  val api = system.actorOf(Props(new TescoApiActor(uri, credentials) with Authentication {
    override def login(uri: Uri, credentials: Credentials): Future[Either[String, SessionId]] = {
      import system.dispatcher
      Future(Right("ZIcFseDfY15SP6cp69ozS7g1GlyCbfuFLBDZhOXBEVzUvmqmsK"))
    }
  }))

  import akka.actor.ActorDSL._
  import TescoApiActor._
  implicit val _ = actor(new Act {
    become {
      case x => println(x)
    }
  })

  def commandLoop(): Unit = {
    Console.readLine() match {
      case "exit" => return
      case query  => api ! Search(query)
    }

    commandLoop()
  }

  commandLoop()

}
