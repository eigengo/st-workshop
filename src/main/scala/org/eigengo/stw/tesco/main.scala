package org.eigengo.stw.tesco

import akka.actor.{Props, ActorSystem}
import spray.http.Uri
import scala.io.Source

object Main extends App {
  import akka.actor.ActorDSL._
  import TescoApiActor._
  implicit val system = ActorSystem()

  implicit val _ = actor(new Act {
    become {
      case x => println(x)
    }
  })
  val uri = Uri("http://www.techfortesco.com/groceryapi_b1/restservice.aspx")
  val credentials = {
    val lines = Source.fromFile(System.getProperty("user.home") + "/.tescoapi").getLines().toList
    Credentials(lines(0), lines(1), lines(2), lines(3))
  }
  val api = system.actorOf(Props(new TescoApiActor(uri, credentials) with RealApiAuthentication))

  val SearchCommand     = "find (\\d+)? (.*)".r
  val ListOffersCommand = "offers (\\d+)?".r

  def commandLoop(): Unit = {
    Console.readLine() match {
      case "exit" => return
      case SearchCommand(page, query) => api ! Search(query, if (page.isEmpty) None else Some(page.toInt))
      case ListOffersCommand(page)    => api ! ListOffers(if (page.isEmpty) None else Some(page.toInt))
    }

    commandLoop()
  }

  commandLoop()

}
