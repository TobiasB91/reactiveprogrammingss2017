import io.circe._
import io.circe.generic.semiauto._

object Codec {
  /////////////////////////////////////////////////////////////////////////////
  // Der folgende Code dient der JSON (De)serialisierung und                 //
  // ist nicht wichtig für das lösen der Aufgabe.                            //
  
  implicit val decodeMaze: Decoder[Maze] = Decoder[Seq[Seq[Boolean]]].map { fields =>
    Maze(width = fields.headOption.map(_.length).getOrElse(0),
         height = fields.length,
         blocked = pos => fields.lift(pos.y).flatMap(_.lift(pos.x)).getOrElse(false)) }  
  
  implicit val decodePosition = deriveDecoder[Position]
  implicit val decodeGameState = deriveDecoder[GameState]  
  implicit val decodeNewGame = deriveDecoder[Messages.Server.NewGame]  
  implicit val decodePositionUpdate = deriveDecoder[Messages.Server.PositionUpdate]  
  implicit val decodeShowMessage = deriveDecoder[Messages.Server.ShowMessage]
  implicit val decodeServerMessage: Decoder[Messages.ServerMessage] =
    decodeNewGame or
    decodePositionUpdate.map[Messages.ServerMessage](x => x) or 
    decodeShowMessage.map(x => x)

  implicit val encodeDirection = Encoder.instance[Direction] ( 
    dir => Encoder.encodeString(dir.toString) )
  implicit val encodeMove = deriveEncoder[Messages.Client.Move]
  implicit val encodeClientMessage: Encoder[Messages.ClientMessage] = Encoder.instance {
    case mv: Messages.Client.Move => encodeMove(mv)
  }
}