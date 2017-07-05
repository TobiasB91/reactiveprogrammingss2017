import scala.scalajs.js.{JSApp, JSON, typeOf}
import org.scalajs.dom.ext.{KeyCode, KeyValue}
import io.circe.parser.decode
import io.circe.syntax._
import Util._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Merge, Sink, Source}
import org.scalajs.dom.{Event, KeyboardEvent, MessageEvent, WebSocket, console, document, html, window}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

case class ClientState(
  ot: Client[TextOperation.type],
  editor: Editor
)
object ClientState {
  def empty = ClientState(Client.empty(TextOperation),Editor.empty)
}

sealed trait ClientEvent
case class Keystroke(key: String) extends ClientEvent
case class Receive(msg: ServerMessage) extends ClientEvent

object Main extends JSApp {
  implicit val system = ActorSystem("ot-client")
  implicit val materializer = ActorMaterializer()

  def main() = {
    window.document.once[Event]("readystatechange").foreach { _ =>
      val socket = new WebSocket("ws://" + window.location.host)
      val editorDiv = window.document.getElementById("doc").asInstanceOf[html.Div]

      val messages: Source[ClientEvent,NotUsed] = socket.on[MessageEvent]("message")
        .map(e => e.data.toString)
        .map(decode[ServerMessage](_))
        .mapConcat {
          case Right(msg) => 
            List(Receive(msg))
          case Left(err) =>
            console.error("could not decode server message", err.getMessage)
            Nil
        }

      val keystrokes: Source[ClientEvent,NotUsed] = window.on[KeyboardEvent]("keydown")
        .mapConcat { e =>
          if (e.metaKey || e.ctrlKey) Nil
          else {
            e.stopPropagation()
            List(Keystroke(e.key))
          }
        }

      val events = Source.combine(messages,keystrokes)(Merge.apply(_))

      val editor = Flow[ClientEvent].scan((ClientState.empty,Option.empty[ClientMessage])) {
        case ((state,lastMessage),Receive(message)) => message match {
          case Ack =>
            val (op, newClient) = state.ot.ack
            val message = op.map(ClientEdit(newClient.revision, _, 1))
            (state.copy(ot = newClient), message)
          case HelloGuys =>
            val newCursors = state.editor.cursors :+ 0 
            (ClientState(state.ot, Editor(state.editor.content, state.editor.cursor, newCursors)), None)
          case RemoteEdit(op, cursors) =>
            val (top, newClient) = state.ot.remoteEdit(op)
            val newContent = TextOperation.applyOp(state.editor.content, top)
            val newCursorPos = TextOperation.transformCursor(op, state.editor.cursor)
            val pendingCursors = state.ot.pending match {
              case None => cursors
              case Some(o) => cursors.map(c => TextOperation.transformCursor(o,c))
            }
            val newCursors = state.ot.buffer match  {
                case None => cursors
                case Some(o) => cursors.map(c => TextOperation.transformCursor(o,c))
            }
            val newState = state.copy(
              ot = newClient,
              editor = Editor(newContent, Math.min(newContent.length, newCursorPos), newCursors)
            )
            (newState, None)
        }
        case ((state,lastMessage),Keystroke(key)) =>
          val (op, newEditor) = key match {
            case KeyValue.Backspace => state.editor.backspace
            case KeyValue.ArrowLeft => state.editor.moveLeft
            case KeyValue.ArrowRight => state.editor.moveRight
            case KeyValue.Enter => state.editor.insert('\n')
            case other if other.length == 1 => state.editor.insert(other.head)
            case other => (None, state.editor)
          }
          val (newClient,message) =
            op.fold(
              (state.ot,Option.empty[ClientMessage])
            ) { op =>
              val (synced,newState) = state.ot.localEdit(op)
              val msg = if (synced) Some(ClientEdit(newState.revision,op, newEditor.cursor)) else None
              (newState,msg)
            }
          (ClientState(newClient,newEditor),message)
      }.recover {
        case NonFatal(e) =>
          (ClientState(Client.empty(TextOperation), Editor(e.getMessage,0,List())),None)
      }

      val sink = Sink.foreach[(ClientState,Option[ClientMessage])] { case (state,msg) =>
        msg.foreach(msg => socket.send(msg.asJson.noSpaces))
        state.editor.render(editorDiv)
      }

      (events via editor to sink).run()
    }
  }
}
