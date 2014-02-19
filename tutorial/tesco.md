```scala
case class Credentials(email: String, password: String, developerKey: String, applicationKey: String)
```

Then, to authenticate, we use:

```scala
trait Authentication {
  type SessionId = String
  type ErrorMessage = String

  def login(uri: Uri, credentials: Credentials): 
    Future[Either[ErrorMessage, SessionId]]
}
```

Now on to the actor and the companion object. A companion object is a type with the same name, but defined as ``object`` instead of ``class`` or ``trait``. In case of actor-based application, this usually contains the messages we send to the actor.

```scala
object TescoApiActor {

  case object Login

  case class Search(query: String, page: Option[Int])

  case class ListOffers(page: Option[Int])

  case object NotLoggedIn

  case class LoggedIn(sessionId: String) extends AnyVal

}
```

Finally, we're ready to have the actor. Its constructor takes the ``uri`` where the Tesco API lives, and the ``credentials`` we will use to log in. Notice that we use mixin inheritance, which feels like multiple implementation inheritance, but does not suffer from the diamond problem. Notice also that we use self-type annotation (``this: Authentication =>``), which says that concrete instances of this class must mix in a proper implementation of the ``Authentication`` trait.

```scala
class TescoApiActor(uri: Uri, credentials: Credentials) 
  extends Actor 
  with DefaultJsonProtocol 
  with SprayJsonSupport 
  with ContentTypeDirectives {
  this: Authentication =>
  
  import TescoApiActor._
  import commands._
  import akka.pattern.pipe
  import context.dispatcher

  // unmarshalling support
  implicit val ProductFormat = jsonFormat3(Product)
  implicit val ProductSearchFormat = jsonFormat2(ProductSearchResponse)

  // low-level IO
  val productSearch = sendReceive ~> 
                      forceApplicationJson ~> 
                      unmarshal[ProductSearchResponse]

  // login when constructing the actor instance
  doLogin()

  /**
   * Performs the login operation
   * @return the future of the result
   */
  private def doLogin() = login(uri, credentials).map {
    case Left(error) => context.become(receive); NotLoggedIn
    case Right(sessionId) => context.become(loggedIn(sessionId)); LoggedIn(sessionId)
  }

  /**
   * Implements behaviour when logged in.
   *
   * @param sessionId the session id
   * @return the actor behaviour
   */
  private def loggedIn(sessionId: String): Receive = {
    def command(command: String, page: Option[Int], 
                queryParameters: (String, String)*) = {
      val params = Map("command" -> command, 
                       "sessionkey" -> sessionId) ++ 
                   queryParameters ++
                   page.map(p => Map("page" -> p.toString)).getOrElse(Map())
      Get(uri.withQuery(params))
    }

    {
      case Search(query, page) => 
        productSearch(command(ProductSearch, page, ("searchtext", query))) pipeTo sender
      case ListOffers(page)    => 
        productSearch(command(ListProductOffers, page)) pipeTo sender
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
```

To complete the picture, we deinfe the ``ContentTypeDirectives``.

```scala
trait ContentTypeDirectives {
  def forceApplicationJson(response: HttpResponse): HttpResponse = 
    response.mapEntity(entity => 
                       HttpEntity(ContentTypes.`application/json`, entity.data))
}
```

Finally, we work on the real implementation of the ``Authentication`` trait:

```scala
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
```

---

Next up, we wire everything in the main application.

```scala
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
    // the ~/.tescoapi file should contain four lines:
    //
    // email address
    // password
    // developer key
    // application key
    //
    val lines = Source.fromFile(System.getProperty("user.home") +
                 "/.tescoapi").getLines().toList
    Credentials(lines(0), lines(1), lines(2), lines(3))
  }
  val api = system.actorOf(
              Props(new TescoApiActor(uri, credentials) with RealApiAuthentication))

  val SearchCommand     = "find (\\d+)? (.*)".r
  val ListOffersCommand = "offers (\\d+)?".r

  /**
   * Main command loop
   */
  @tailrec
  def commandLoop(): Unit = {
    Console.readLine() match {
      case "exit" => return
      case SearchCommand(page, query) => 
        api ! Search(query, if (page.isEmpty) None else Some(page.toInt))
      case ListOffersCommand(page) => 
        api ! ListOffers(if (page.isEmpty) None else Some(page.toInt))
    }

    commandLoop()
  }

  commandLoop()

}
```
