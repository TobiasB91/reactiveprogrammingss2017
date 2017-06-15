import java.util.concurrent.TimeoutException

import org.scalajs.dom.raw.MessageEvent
import org.scalajs.dom.{Event, WebSocket, window}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.JavaScriptException
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * A wrapper around a websocket which handles timestamped messages.
  */
trait MessageSocket {
  def onMessage(msg: TimestampedMessage => Unit): Unit
  def onClose(f: => Unit): Unit
  def onError(f: Throwable => Unit): Unit
  def send(f: Message): Unit
}

object MessageSocket {
  /**
    * Opens a websocket connection and returns a future of the opened connection.
    */
  def open(path: String = "", timeout : Int = 2000): Future[MessageSocket] = {
    val p = Promise[WebSocket]
    val s = new WebSocket("ws://" + window.location.host + path)
    s.onopen = _ => p.success(s)
    s.onerror = e => p.tryFailure(new JavaScriptException(e))
    val t = Promise[WebSocket]
    window.setTimeout(() => t.failure(new TimeoutException(s"websocket did not open within $timeout ms")), timeout)
    Future.firstCompletedOf(Seq(p.future,t.future)).map { socket =>
      import io.circe.parser._
      import io.circe.syntax._
      new MessageSocket {
        override def onError(f: (Throwable) => Unit): Unit =
          socket.addEventListener("error",f)
        override def onClose(f: => Unit): Unit =
          socket.addEventListener("close",(_: Event) => f)
        override def onMessage(f: (TimestampedMessage) => Unit): Unit =
          socket.addEventListener("message",(e: MessageEvent) => {
            decode[TimestampedMessage](e.data.toString).fold(
              err => window.console.error("could not decode message", e.data.toString, err.getMessage)
            ,
              m => f(m)
            )

          })
        override def send(msg: Message): Unit = {
          socket.send(TimestampedMessage(msg).asJson.noSpaces)
        }
      }
    }
  }
}