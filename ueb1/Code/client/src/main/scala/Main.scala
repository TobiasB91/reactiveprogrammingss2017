import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import io.circe.parser._
import Messages._

/**
 * Die JavaScript Client Anwendung
 */
object Main extends JSApp {
  import Codec._

  def main(): Unit = {
    // Wir bauen eine Websocket Verbindung zum Server Auf
    val socket = new WebSocket("ws://" + window.location.host)
    
    // Hier reagieren wir auf Nachrichten die wir vom Server bekommen
    socket.onmessage = (e) => {
      // Die dekodierte JSON Nachricht
      val msg = decode[ServerMessage](e.data.toString)
      // Wenn ein Fehler beim dekodieren aufgetreten ist, werfe ihn auf der Konsole
      // Sonst verarbeite die Nachricht
      msg.fold(throw _, {
        case Server.NewGame(GameState(maze,pos,target)) =>          
          renderMaze(maze,target)
          renderPlayer(pos.x,pos.y)
        case Server.PositionUpdate(Position(x,y)) =>
          renderPlayer(x,y)
        case Server.ShowMessage(message) =>
          window.alert(message)
      })
    }

    // Wenn die Verbindung geschlossen wird, zeigen wir eine Fehlermeldung an.
    socket.onclose = (e) => {      
      document.getElementById("maze").innerHTML = 
        "<span class='error'>Die Verbindung zum Server wurde geschlossen</span>"
    }

    // Hier verarbeiten wir Tastatureingaben und leiten die Kommandos an den
    // Server Weiter (socket.send)
    window.onkeypress = (e) => {
      val msg = keyToCommand.lift(e.keyCode).map(encodeClientMessage(_).noSpaces)
      msg.foreach(socket.send)
    }
  }

  /**
   * Rendert das Labyrinth.
   */
  def renderMaze(maze: Maze, target: Position) = {
    document.getElementById("mazeStructure").innerHTML = (0 until maze.height).map { y =>
      (0 until maze.width).map { x =>
        val pos = Position(x,y)
        val cellClass = 
          if (pos == target) "target"
          else if (maze.blocked(pos)) "blocked" else "free"        
        s"<div class='cell $cellClass'></div>"
      }.mkString("<div class='row'>","","</div>")
    }.mkString
  }

  /**
   * Setzt die Spielerposition
   */
  def renderPlayer(x: Int, y: Int) = {
    val div = document.getElementById("player").asInstanceOf[html.Div]
    div.style.left = (16 * x) + "px"
    div.style.top = (16 * y) + "px"
  }

  /**
   * Übersetzt einen Tastencode in eine Kommando
   */
  def keyToCommand: PartialFunction[Int,ClientMessage] = {
    case KeyCode.Up => Client.Move(Direction.North)
    case KeyCode.Left => Client.Move(Direction.West)
    case KeyCode.Right => Client.Move(Direction.East)
    case KeyCode.Down => Client.Move(Direction.South)
  }  
}