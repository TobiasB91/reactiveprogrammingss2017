module Main where

import Data.Aeson (encode,decode)
import Data.String
import Data.Maybe (maybe)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import System.Random (newStdGen)

import Maze
import Messages
import Codec

-- Beginne ein neues Spiel auf einer WebSocket Verbindung
startGame :: WS.Connection -> IO ()
startGame conn = do
  let send msg = WS.sendTextData conn (encode msg)
  -- Initialisiere einen Zufallsgenerator
  -- Erstelle ein Spiel mit einem 24x24 großen Labyrinth
  game <- newGame 24
  -- Schicke den initialen Spielzustand an den Client
  send $ NewGame game
  -- Warte auf Anweisungen vom Client
  handleSocket conn game

handleSocket :: WS.Connection -> GameState -> IO ()
handleSocket conn game = do
  let send msg = WS.sendTextData conn (encode msg)
  -- Empfange die Daten vom Client
  msg <- WS.receiveData conn
  -- Konvertiere den String zu einer Anweisung
  -- und Wende die Anweisung auf den Spielzustand an
  let game' = case decode msg of {
    Just (Move dir) -> move game dir;
    otherwise       -> game
  }
  -- Wenn das Spiel gewonnen wurde, starte ein neues
  -- ansonsten schicke den aktuellen Zustand und warte
  -- auf die nächste Nachricht
  if won game'
    then do
      send $ ShowMessage "Gewonnen!"
      startGame conn
    else do
      send $ PositionUpdate (position game')
      handleSocket conn game'

----------------------------------------------------------------------------

-- Die folgenden funktionen dienen dem generellen Setup der Server Anwendung
-- und sind für die Lösung der Aufgabe nicht relvant.


-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp staticSettings
staticSettings = (Static.defaultFileServerSettings "client/assets")
  -- Caching ausschalten, damit die Anwendung nicht neu gestartet werden
  -- muss, wenn der Scala teil neu Kompiliert wurde.
  { Static.ssMaxAge = Static.NoMaxAge }

-- Der WebSocket
socket pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  startGame conn

-- Starte die Webanwendung auf Port 3000
main = Warp.run 3000 $
  WS.websocketsOr WS.defaultConnectionOptions socket static
