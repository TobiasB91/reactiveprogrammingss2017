{-# LANGUAGE LambdaCase #-}
module Universe where

import Prelude hiding (log)
import Hakka.Actor
import Connection
import Messages
import Control.Monad
import System.Random
import Control.Concurrent

universe :: ActorIO ServerMessage (ActorRef ServerMessage)
universe = actor "universe" $ initialize 0 where
  initialize idCount = \case 
    Msg (TimestampedMessage t Init) -> do
      asteroids <- forM [idCount..idCount+9] $ \index -> do 
        rnds <- liftIO . replicateM 8 $ randomRIO (0,1.0) 
        let pos = (500 - (rnds !! 0) * 1000, 500 - (rnds !! 1) * 1000) 
        let omega = (rnds !! 2) * pi * 2 
        let phi = (rnds !! 3) - 0.5 
        let velo = (50 - (rnds !! 4) * 100, 50 - (rnds !! 5) * 100) 
        let size = case round ((rnds !! 6) * 4) of 
              0 -> Tiny
              1 -> Small
              2 -> Medium
              _ -> Big
        let color = case round ((rnds !! 7) * 2) of 
              0 -> Brown
              _ -> Gray 
        asteroid index pos velo 0 omega phi size color 
      become $ receive (idCount + length asteroids) [] asteroids 
      forM_ asteroids (! (Msg (TimestampedMessage t Update)))
  receive idCount connections asteroids = \case
    Connect out res -> do
      connection <- connection $ length connections      
      forward connection (Connect out res)
      become $ receive (idCount+1) (connection:connections) asteroids 
    update@(Msg (TimestampedMessage t (Asteroid asteroid size color))) -> do 
      become $ receive idCount connections asteroids 
      forM_ connections (! update) 

asteroid :: Int -> (Double,Double) -> (Double, Double) -> Double -> Double -> Double -> ASize -> AColor -> ActorIO ServerMessage (ActorRef ServerMessage)
asteroid id p@(px,py) v@(vx,vy) a omega phi size color = actor (show id) (curState p v omega) where
  curState (px,py) (vx,vy) omega = \case 
    Msg (TimestampedMessage prevTime Update) -> do  
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Asteroid (CommonState id p' v' 0 omega' phi) size color)))
      liftIO $ threadDelay 50000
      self >>= (! (Msg (TimestampedMessage time Update)))
      become $ curState p' v' omega'
