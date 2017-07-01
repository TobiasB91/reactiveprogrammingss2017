{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.String
import Data.Maybe (maybe)
import Control.Concurrent.MVar
import Control.Monad (forM_)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Map.Lazy as M
import System.Random (newStdGen)
import Messages
import OT
import Control.Exception (catch, throw)

data ServerState = ServerState {
  ot :: Server TextOperation String,
  idGen :: ClientId,
  clients :: [(ClientId,WS.Connection)],
  cursors :: M.Map ClientId Cursor
}

type ClientId = Int

initialState = ServerState {
  ot = (Server ("" :: [Char]) ([] :: [TextOperation])),
  idGen = 0,
  clients = [],
  cursors = M.empty
}

handleSocket :: WS.Connection -> ClientId -> Int -> MVar ServerState -> IO ()
handleSocket conn id offset server = do
  -- Empfange die Daten vom Client
  let disconnect err = do
        putStrLn $ "disconnected:  client " ++ show id
        modifyMVar_ server $ \s ->
          return s { clients = filter ((/= id). fst) (clients s), 
            cursors = M.delete id $ cursors s }
        throw (err :: WS.ConnectionException)
  msg <- catch (WS.receiveData conn) disconnect
  case decode msg of
    Just (ClientEdit rev op cursor) -> do
      print cursor
      op' <- modifyMVar server $ \s -> do
        let (op',ot') = appendOperation (ot s) op (offset + rev)
        let newCursor = foldl (flip transformCursor) cursor $ drop rev $ history . ot $ s
        let newOtherCursors = M.map (transformCursor op) $ cursors s 
        let newCursors = M.insert id newCursor newOtherCursors
        forM_ (clients s) $ \(cid,client) -> do
          let cursors' = M.filterWithKey (\k _ -> cid/=k) newCursors
          if cid /= id
          then do
            WS.sendTextData client (encode (RemoteEdit op' cursors'))
            print cursors'
          else WS.sendTextData client (encode Ack)
        return (s { ot = ot', cursors = newCursors },op')
      handleSocket conn id offset server
    Nothing -> do
      print msg
      error "could not decode client message"

-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp (staticSettings)
staticSettings = (Static.defaultFileServerSettings "client/assets")
  -- Caching ausschalten, damit die Anwendung nicht neu gestartet werden
  -- muss, wenn der Scala teil neu Kompiliert wurde.
  { Static.ssMaxAge = Static.NoMaxAge }

-- Der WebSocket
socket server pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  (id,offset) <- modifyMVar server $ \s -> do
    let id = 1 + idGen s
    let clients' = (id,conn) : clients s
    let offset = serverRevision (ot s) - 1
    let stateOp = serverStateOp (ot s)
    let cursors' = M.insert id 0 $ cursors s
    WS.sendTextData conn (encode (RemoteEdit stateOp $ cursors s))
    forM_ (clients s) $ \(_, con) -> 
      WS.sendTextData con $ encode HelloGuys 
    return (s {idGen = id, clients = clients', cursors = cursors'}, (id,offset))
  putStrLn $ "connected: client " ++ show id
  handleSocket conn id offset server

-- Starte die Webanwendung auf Port 3000
main = do
  server <- newMVar $ Main.initialState
  Warp.run 3000 $
    WS.websocketsOr WS.defaultConnectionOptions (socket server) static
