module Codec where 

import Data.Aeson
import Messages
import Maze

-----------------------------------------------------------------------------
-- Der folgende Code dient der JSON (De)serialisierung und                 --
-- ist nicht wichtig für das lösen der Aufgabe.                            --
  
instance ToJSON Position
instance ToJSON Maze where
  toJSON (Maze width height blocked) = toJSON $ map row [0..height-1]
    where row y = map (blocked . flip Position y) [0..width-1]
instance ToJSON GameState
instance ToJSON ServerMessage

instance FromJSON Direction
instance FromJSON ClientMessage