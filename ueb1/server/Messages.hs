{-# LANGUAGE DeriveGeneric #-}

module Messages (
  ServerMessage (NewGame, PositionUpdate, ShowMessage),
  ClientMessage (Move)
) where

import Maze
import GHC.Generics

-- Nachrichten die vom Server zum Client gesendet werden

data ServerMessage = NewGame { state :: GameState }
                   | PositionUpdate { pos :: Position }
                   | ShowMessage { text :: String } deriving (Generic)

-- Nachrichten die vom Client zum Server gesendet werden

data ClientMessage = Move { direction :: Direction } deriving (Generic)