module Actors (ActorRef(..), ActorContext, Behavior (..), self, 
  sender, actor, ask, send, root, respond, stop, failed, become) where

import System.IO
import Control.Exception (try, evaluate, SomeException)
import Control.Concurrent

data Message a = Failed String | Stop | Message (ActorRef a) a 

data ActorRef a = ActorSystem (ActorRef a) | ActorRef (ActorRef a) (MVar (Message a))

instance Eq (ActorRef a) where
  ActorRef _ var == ActorRef _ var' = var == var'
  ActorSystem a == ActorSystem b = a == b
  _ == _ = False 

data ActorContext a = ActorContext {
  self :: ActorRef a,
  sender :: ActorRef a
}

newtype Behavior a = Behavior {
  receive :: ActorContext a -> a -> IO (Behavior a)
}

inbox :: ActorRef a -> MVar (Message a)
inbox (ActorRef _ var) = var
inbox (ActorSystem ref) = inbox ref

root :: ActorRef a -> ActorRef a
root (ActorRef parent _) = root parent 
root r = r

respond :: ActorContext a -> a -> IO ()
respond context = send (sender context) (self context)

send :: ActorRef a -> ActorRef a -> a -> IO ()
send recipient sender message = do
  let recipient' = inbox recipient
  forkIO $ putMVar recipient' (Message sender message)
  return ()

ask :: ActorRef a -> a -> IO a
ask recipient message = do
  inbox <- newEmptyMVar
  let self = ActorRef undefined inbox
  send recipient self message
  (Message sender answer) <- takeMVar inbox
  return answer

stop :: ActorRef a -> IO ()
stop recipient = do
  let recipient' = inbox recipient
  forkIO $ putMVar recipient' Stop
  return ()

failed :: ActorRef a -> Message a -> IO ()
failed recipient msg = do
  let recipient' = inbox recipient 
  forkIO $ putMVar recipient' msg
  return ()

become :: (ActorContext a -> a -> IO (Behavior a)) -> IO (Behavior a)
become = return . Behavior

actor :: Maybe (ActorRef a) -> Behavior a -> IO (ActorRef a)
actor parent behavior = do
  inbox <- newEmptyMVar
  let self = case parent of 
        Nothing -> ActorSystem $ ActorRef undefined inbox
        Just p  -> ActorRef p inbox
  let loop (Behavior behavior) = do
        msg <- takeMVar inbox
        case msg of
          Stop -> return ()
          Message sender msg -> do
            let context = ActorContext self sender
            result <- try . evaluate $ behavior context msg
            case result of
              Right newState -> do 
                fromIO <- newState
                loop fromIO 
              Left err -> case parent of 
                Just parent -> failed parent $ Failed . show $ (err :: SomeException) 
                Nothing -> return ()
          Failed err -> do
            hPutStrLn stderr err 
            loop $ Behavior behavior 
  forkIO $ loop behavior
  return self
