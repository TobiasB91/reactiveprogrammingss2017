import akka.actor._

object Universe {
  case object Connect
}

class Universe extends Actor with ActorLogging {
  def receive = {
    case Universe.Connect =>
      val connection = context.actorOf(Props[Connection])
      log.info("created connection")
      sender ! connection
  }
}