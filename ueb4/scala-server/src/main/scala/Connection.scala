import akka.actor._

object Connection {
  case class Start(client: ActorRef)
  case object Stop
}

class Connection extends Actor with ActorLogging {
  def receive = {
    case Connection.Start(client) =>
      log.info("new client connected")
      context.become(connected(client))
  }

  def connected(client: ActorRef): Receive = {
    case TimestampedMessage(t,Ping) =>
      println(t,Ping)
      client ! TimestampedMessage(Pong)
    case Connection.Stop =>
      log.info("connection closed")
      context.stop(self)
  }
}