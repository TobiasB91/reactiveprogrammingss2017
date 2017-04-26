import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import io.circe.parser._
import Messages._
import scala.collection.immutable.Queue
import scala.collection.Seq

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
          window.setTimeout(() => { 
            solveMaze(maze, pos, target)
          }, 1000)
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
    window.onkeydown = (e) => {
      val msg = keyToCommand.lift(e.keyCode).map(encodeClientMessage(_).noSpaces)
      msg.foreach(socket.send)
    }
  
    def sendPath(path : Seq[Direction]) : Unit = {
      path.headOption match {
        case None => ()
        case Some(d) => window.setTimeout(() => {
          socket.send(encodeClientMessage(Client.Move(d)).noSpaces)
          sendPath(path.tail)
        },400)
      }
    }
    
    def solveMaze(maze: Maze, start: Position, target : Position) : Unit = {
      solve(maze, start, target) match {
        case None => window.alert("Maze not solveable")
        case Some(path) => sendPath(path)
      }
    }


    def solve(maze : Maze, start : Position, target : Position): Option[Seq[Direction]] = {
      val visited = Set[Position]()
      val q = Queue[(Seq[Direction], Position)]((Seq(),start))
      shortestPath(q, visited, target, maze) 
    }

    def shortestPath(q : Queue[(Seq[Direction], Position)], visited : Set[Position],
     target : Position, maze : Maze) : Option[Seq[Direction]] = {
      q match {
      case q : Queue[(Seq[Direction], Position)] if q.isEmpty => None
      case _ => 
        val ((path, cur), qq) = q.dequeue 
        if (cur == target) {
          Some(path)
        } else if (visited contains cur) {
          shortestPath(qq, visited, target, maze) 
        } else { 
          val newVisited = visited + cur
          val qqq = qq ++ (for (
            (dir, pos) <- neighbours(cur, maze)
          ) yield {
            (path :+ dir, pos)
          })
          shortestPath(qqq, newVisited, target, maze) 
        }
      }
    }
  
    def neighbours(p : Position, maze : Maze) : Seq[(Direction, Position)] = {
      for (i <- Seq.range(-1,2); 
           ii <- Seq.range(-1,2); 
           if (((i != 0 || ii != 0) && !(i != 0 && ii != 0)) && 
             !(maze.blocked(Position(p.x+i, p.y+ii))) && p.x+i >= 0 
           && p.x+i < maze.width && p.y+ii >= 0 && p.y+ii < maze.height)
      ) yield { 
          val pp = Position(p.x+i,p.y+ii)
          (i, ii) match {
            case (1,0) => (Direction.East, pp)
            case (-1,0) => (Direction.West, pp)
            case (0,-1) => (Direction.North, pp)
            case (0,1) => (Direction.South, pp)
            case _ => throw new Exception("should not happen")
          }
      }
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
