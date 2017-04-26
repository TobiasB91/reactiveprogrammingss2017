object Messages {  
  // Nachrichten die vom Server zum Client gesendet werden
  sealed trait ServerMessage

  object Server {
    case class NewGame(state: GameState) extends ServerMessage
    case class PositionUpdate(pos: Position) extends ServerMessage
    case class ShowMessage(text: String) extends ServerMessage  
  }

  // Nachrichten die vom Client zum Server gesendet werden
  sealed trait ClientMessage

  object Client {
    case class Move(direction: Direction) extends ClientMessage
  }
}