{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (log)
import Data.Aeson (encode,decode)
import Data.String
import Data.Maybe (maybe)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import Control.Concurrent.MVar
import System.Random (newStdGen)
import Hakka.Actor
import Control.Exception

import Messages
import Universe

type ConnectionState = ()
type UniverseState = ()

handleSocket :: ActorSystem ServerMessage -> ActorRef ServerMessage -> WS.Connection -> ConnectionState -> IO ()
handleSocket system actor conn state = catch handleMessage handleClose where
  send msg = WS.sendTextData conn (encode msg)
  handleClose e = case e of
    WS.CloseRequest _ _ ->
      tellIO system actor $ Disconnect undefined
    WS.ConnectionClosed ->
      tellIO system actor $ Disconnect undefined
    other ->
      throw other
  handleMessage = do
    msg <- WS.receiveData conn
    case (decode msg) of
      Nothing -> do
        putStrLn $ "invalid message from client: '" ++ (show msg) ++ "'"
        handleSocket system actor conn state    
      Just tmsg -> do
        tellIO system actor $ Msg tmsg
        handleSocket system actor conn state  
  
-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp (staticSettings)
staticSettings = (Static.defaultFileServerSettings "scala-client/assets")
  -- Caching ausschalten, damit die Anwendung nicht neu gestartet werden
  -- muss, wenn der Scala teil neu kompiliert wurde.
  { Static.ssMaxAge = Static.NoMaxAge }

-- Der WebSocket
socket system universe pending = do
  conn <- WS.acceptRequest pending
  connectionActor <- newEmptyMVar
  tellIO system universe $ Connect (WS.sendTextData conn . encode) connectionActor
  connectionActor <- takeMVar connectionActor
  WS.forkPingThread conn 30
  handleSocket system connectionActor conn ()

-- Starte die Webanwendung auf Port 3000
main = do
  (system,universe) <- actorSystem "curried-in-space" $ do
    universe <- universe
    initMsg <- liftIO $ timestamped Init 
    universe ! Msg initMsg
    return universe
  Warp.run 3000 $
    WS.websocketsOr WS.defaultConnectionOptions (socket system universe) static    
  _ <- getLine
  terminate system
