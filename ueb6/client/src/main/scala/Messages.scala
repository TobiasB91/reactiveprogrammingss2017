import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import org.scalajs.dom.{document, html, console}

sealed trait ClientMessage
case class ClientEdit(
  rev: Int,
  op: TextOperation.Operation,
  cursor: Int) extends ClientMessage

sealed trait ServerMessage
case object Ack extends ServerMessage
case object HelloGuys extends ServerMessage 
case class RemoteEdit(
  op: TextOperation.Operation,
  cursors: List[Int]
) extends ServerMessage

object ClientMessage {
  implicit val encodeAction =
    Encoder.instance[TextAction] {
      case Retain(n) => n.asJson
      case Insert(cs) => cs.asJson
      case Delete(n) => (-n).asJson
    }

  implicit val encodeClientMessage = Encoder.instance[ClientMessage] {
    case ClientEdit(rev,op,cursor) => Json.obj(
      "rev" -> rev.asJson,
      "op" -> op.asJson,
      "cursor" -> cursor.asJson
    )
  }
}

object ServerMessage {
  implicit val parseAction =
    Decoder.decodeString.map[TextAction](Insert) or
    Decoder.decodeInt.map[TextAction] { n => 
      if (n < 0)
        Delete(-n)
      else  
        Retain(n)
    }

  val parseAckAndHello = Decoder.decodeString.emap[ServerMessage] {
    case "ack" => Right(Ack)
    case "helloguys" => Right(HelloGuys)
    case other => Left("invalid server message")
  }


  val parseRemoteEdit : Decoder[ServerMessage] = 
    Decoder.instance( o =>
      for {
        op <- o.downField("op").as[List[TextAction]] 
        cursors <- o.downField("cursors").as[List[Int]]
      } yield RemoteEdit(op,cursors)
    )

  implicit val parseServerMessage =
    parseAckAndHello or parseRemoteEdit
}
