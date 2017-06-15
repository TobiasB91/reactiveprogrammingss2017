import akka.NotUsed
import akka.actor._
import akka.http.scaladsl._
import akka.http.scaladsl.model.ws.{TextMessage, Message => WSMessage}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream._
import akka.stream.scaladsl._
import akka.util.Timeout
import io.circe.parser._
import io.circe.syntax._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.io.StdIn

object Main extends App {
  implicit val system = ActorSystem("akka-system")
  implicit val materializer = ActorMaterializer()
  implicit val dispatcher = system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  val universe = system.actorOf(Props[Universe])

  def socket: Flow[WSMessage,WSMessage,Any] = {
    val connection: Sink[TimestampedMessage, Future[ActorRef]] = Sink.lazyInit[TimestampedMessage,ActorRef]({ t =>
      (universe ? Universe.Connect).mapTo[ActorRef].map { connection =>
        Sink.actorRef[TimestampedMessage](connection,Connection.Stop)
            .mapMaterializedValue(_ => connection)
      }
    }, fallback = () => system.deadLetters)

    val in = Flow[WSMessage].collect {
        case TextMessage.Strict("pong") => None
        case TextMessage.Strict(text) =>
          val decoded = decode[TimestampedMessage](text)
          decoded.left.foreach { e =>
            println(e)
          }
          decoded.toOption
      }
      .collect { case Some(msg) => msg }
      .toMat(connection)(Keep.right)

    val out: Source[TimestampedMessage, ActorRef] = Source.actorRef[TimestampedMessage](1, OverflowStrategy.fail)

    Flow.fromSinkAndSourceMat(in,out)(Keep.both)
      .map(msg => TextMessage.Strict(msg.asJson.noSpaces))
      .keepAlive(20 seconds, () => TextMessage.Strict("ping"))
      .idleTimeout(30 seconds)
      .mapMaterializedValue {
        case (in,out) => in.foreach(_ ! Connection.Start(out))
      }
  }

  val route = concat(
    extractUpgradeToWebSocket { _ => handleWebSocketMessages(socket) },
    pathPrefix("js")(getFromResourceDirectory(".")),
    pathSingleSlash(getFromFile("scala-client/assets/index.html")),
    getFromDirectory("scala-client/assets")
  )

  val binding = Http().bindAndHandle(route,"localhost",3000)

  StdIn.readLine()
  binding.flatMap(_.unbind()).onComplete(_ => system.terminate())
}