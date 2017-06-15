{-# LANGUAGE LambdaCase #-}
module Connection where

import Prelude hiding (log)
import Hakka.Actor
import Messages
import Control.Concurrent.MVar
import Control.Concurrent

connection :: Show a => a -> ActorIO ServerMessage (ActorRef ServerMessage)
connection n = actor ("connection" ++ show n) receive where
  receive = \case
    Connect out res -> do
      log Info "new client connected"            
      self >>= liftIO . putMVar res
      become $ connected out
    Disconnect -> do
      log Info "connection closed"
      stop
  connected send = \case
    Msg msg@(TimestampedMessage t (Asteroid _ _ _)) -> do
      become $ connected send 
      liftIO $ print msg
      liftIO $ send msg
    Msg (TimestampedMessage t m) -> do
      log Info $ "received: " ++ show m ++ " at " ++ show t
      case m of
        Ping -> liftIO $ do
          answer <- timestamped Pong
          send answer
      become $ connected send
    
