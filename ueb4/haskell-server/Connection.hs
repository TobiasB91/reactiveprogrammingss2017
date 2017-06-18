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
    Disconnect _ -> do
      univ ! Disconnect n
      log Info "connection closed"
      stop
  connected send = \case
    Msg msg@(TimestampedMessage t m) -> do
      case m of
        Ping -> univ ! (Msg (TimestampedMessage t (ClientId n)))
        Cmd _ cmd -> univ ! (Msg (TimestampedMessage t (Cmd n cmd))) 
        _ -> liftIO $ send msg
      become $ connected send
    Disconnect _ -> do
      univ ! Disconnect n
      log Info "connection closed"
      stop
    
