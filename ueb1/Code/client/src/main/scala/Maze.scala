// Datentypen um Labyrinthe zu beschreiben

/** Eine Position im labyrinth mit x und y Koordinaten */
case class Position (
  x: Int,
  y: Int
)

/**
 * Ein Labyinth
 * @param width Die Breite
 * @param height Die Höhe
 * @param blocked Die blocked funktion gibt an ob eine Position begehbar ist (false) oder nicht (true)
 */
case class Maze (
  width: Int,
  height: Int,  
  blocked: Position => Boolean
)

/**
 * Der Zustand eines Spiels bestehend aus
 * @param maze Das Labyrinth
 * @param position Die aktuelle Spielerposition
 * @param target Die Zielposition
 */
case class GameState(
  maze: Maze,
  position: Position,
  target: Position
)

/** Bewegungsrichtung */
sealed trait Direction
object Direction {
  case object East extends Direction
  case object North extends Direction
  case object West extends Direction
  case object South extends Direction
}