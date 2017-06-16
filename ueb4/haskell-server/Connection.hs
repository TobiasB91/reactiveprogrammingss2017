{-# LANGUAGE LambdaCase #-}
module Connection where

import Prelude hiding (log)
import Hakka.Actor
import Messages
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception

connection :: Int -> ActorRef ServerMessage -> ActorIO ServerMessage (ActorRef ServerMessage)
connection n univ = actor ("connection" ++ show n) receive where
  receive = \case
    Connect out res -> do
      log Info "new client connected"            
      self >>= liftIO . putMVar res
      become $ connected out
    Disconnect -> do
      univ ! Disconnect
      log Info "connection closed"
      stop
  connected send = \case
    Msg msg@(TimestampedMessage t (Asteroid _ _ _)) -> do
      become $ connected send 
      --liftIO $ print msg
      liftIO $ send msg
    Msg msg@(TimestampedMessage t (Spaceship _)) -> do
      become $ connected send 
      liftIO $ send msg
    Msg msg@(TimestampedMessage t (Laser _ _)) -> do
      become $ connected send 
      liftIO $ send msg
    Msg (TimestampedMessage t m) -> do
      log Info $ "received: " ++ show m ++ " at " ++ show t
      case m of
        Ping -> liftIO $ do
          answer <- timestamped Pong
          send answer
        Cmd _ cmd -> univ ! (Msg (TimestampedMessage t (Cmd n cmd))) 
      become $ connected send
    Disconnect -> do
      univ ! Disconnect
      log Info "connection closed"
      stop
    
