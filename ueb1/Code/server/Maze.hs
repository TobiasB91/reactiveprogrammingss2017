{-# LANGUAGE DeriveGeneric #-}

module Maze where

import GHC.Generics
import System.Random

-- Datentypen um Labyrinthe zu beschreiben

-- | Eine Position im labyrinth mit x und y Koordinaten
data Position = Position {
  x :: Int,
  y :: Int
} deriving (Eq,Generic)

-- | Ein Labyinth
-- width Die Breite
-- height Die Höhe
-- blocked Die blocked funktion gibt an ob eine Position begehbar ist (false) oder nicht (true)
data Maze = Maze {
  width :: Int,
  height :: Int,
  -- Die blocked funktion gibt an ob eine Position begehbar ist (false) oder nicht (true)
  blocked :: Position -> Bool
}
  
-- | Der Zustand eines Spiels bestehend aus
-- * maze: Dem Labyrinth
-- * position: Der aktuellen Spielerposition
-- * target: Der Zielposition
data GameState = GameState {
  maze :: Maze,
  position :: Position,
  target :: Position
} deriving (Generic)

-- | Bewegungsrichtung
data Direction = East | North | West | South deriving (Generic)

newMaze :: RandomGen g => g -> Int -> Int -> Maze
newMaze g w h = Maze w h (\_ -> False)

newGame g w h = GameState {
  maze = newMaze g w h, 
  position = Position 0 0,
  target = Position (w-1) (h-1)
}

-- | testet ob das spiel gewonnen wurde
won game = target game == position game

-- | addiert zwei Positionen
addPos :: Position -> Position -> Position
addPos (Position x1 y1) (Position x2 y2) = Position (x1+x2) (y1+y2)

-- | bewegt den spieler in die angegebene richtung
-- wenn die Zielzelle geblockt ist oder außerhalb des Spielfelds liegt,
-- wird der Zustand nicht verändert
move :: GameState -> Direction -> GameState
move (GameState maze position target) dir = (GameState maze pos'' target)
  where mv = case dir of
          East -> Position 1 0
          North -> Position 0 (-1)
          West -> Position (-1) 0
          South -> Position 0 1
        pos' = addPos position mv
        illegal pos = x pos < 0 || y pos < 0 || x pos >= width maze || y pos >= height maze || blocked maze pos
        pos'' = if illegal pos' then position else pos'
