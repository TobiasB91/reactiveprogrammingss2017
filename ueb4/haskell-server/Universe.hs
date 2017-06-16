{-# LANGUAGE LambdaCase #-}
module Universe where

import Prelude hiding (log)
import Hakka.Actor
import Connection
import Messages
import Control.Monad
import System.Random
import Control.Concurrent
import Data.List
import qualified Data.Map.Lazy as Map 

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
      become $ receive (idCount + length asteroids) Map.empty asteroids Map.empty [] 
      forM_ asteroids (! (Msg (TimestampedMessage t Update)))
  receive idCount connections asteroids spaceships lasers = \case
    Connect out res -> do
      self >>= \s -> do 
        connection <- connection idCount s    
        forward connection (Connect out res)
        become $ receive (idCount+1) (Map.insert idCount connection connections) asteroids spaceships lasers 
    Disconnect conId-> do
      sender >>= \con -> do 
        let connections' = Map.delete conId connections 
        become $ receive idCount connections' asteroids spaceships lasers
    Msg (TimestampedMessage t (ClientId cId)) -> 
      case Map.lookup cId connections of
        Just connection -> do
          sp <- spaceship cId (0,0) (0,0) 0 0 0 
          (liftIO $ timestamped (ClientId cId)) >>= \msg -> connection ! Msg msg
          (liftIO $ timestamped Update) >>= \msg -> (sp ! Msg msg)
          become $ receive idCount connections asteroids (Map.insert cId sp spaceships) lasers
        Nothing -> do
          liftIO $ print "Unexpected: connection not avaible" 
          become $ receive idCount connections asteroids spaceships lasers
    update@(Msg (TimestampedMessage t (Asteroid _ _ _))) -> do  
      become $ receive idCount connections asteroids spaceships lasers
      forM_ connections (! update) 
    update@(Msg (TimestampedMessage t (Spaceship _))) -> do 
      become $ receive idCount connections asteroids spaceships lasers
      forM_ connections (! update)
    update@(Msg (TimestampedMessage t (Laser _ _))) -> do 
      become $ receive idCount connections asteroids spaceships lasers
      forM_ connections (! update)
    msg@(Msg (TimestampedMessage t (Cmd sId cmd))) -> do
      case Map.lookup sId spaceships of 
        Just sp -> sp ! msg
        _ -> liftIO $ print "err: not connected anymore" 
      become $ receive idCount connections asteroids spaceships lasers
    msg@(Msg (TimestampedMessage t (CreateLaser sId pos v@(vx,vy) omega))) -> do
      let velo' = add v $ scalarMult (rotate (0,1) omega) 1000 
      let a = 0
      let phi = 0
      laser <- laser idCount sId pos velo' a omega phi 
      laser ! Msg (TimestampedMessage t Update)
      become $ receive (idCount+1) connections asteroids spaceships (laser:lasers) 


asteroid :: Int -> (Double,Double) -> (Double, Double) -> Double -> Double -> Double -> ASize -> AColor -> ActorIO ServerMessage (ActorRef ServerMessage)
asteroid id p@(px,py) v@(vx,vy) a omega phi size color = actor (show id) (receive p v omega) where
  receive (px,py) (vx,vy) omega = \case 
    Msg (TimestampedMessage prevTime Update) -> do  
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Asteroid (CommonState id p' v' a omega' phi) size color)))
      --liftIO $ threadDelay 50000
      --self >>= (! (Msg (TimestampedMessage time Update)))
      self >>= \s -> scheduleOnce 50 s (Msg (TimestampedMessage time Update))
      become $ receive p' v' omega'


spaceship :: Int -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> ActorIO ServerMessage (ActorRef ServerMessage)
spaceship id p@(px,py) v@(vx,vy) aInit omega phiInit = actor (show id) (receive p v aInit omega phiInit) where
 receive (px,py) (vx,vy) a omega phi = \case 
    Msg (TimestampedMessage prevTime Update) -> do  
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Spaceship (CommonState id p' v' a omega' phi))))
      --liftIO $ threadDelay 110000
      --self >>= (! (Msg (TimestampedMessage time Update)))
      self >>= \s -> scheduleOnce 100 s (Msg (TimestampedMessage time Update))
      become $ receive p' v' aInit omega' phiInit
    Msg (TimestampedMessage time (Cmd _ S)) -> do
      parent >>= (! Msg (TimestampedMessage time (CreateLaser id (px,py) (vx,vy) omega)))
      become $ receive (px,py) (vx,vy) a omega phi
    Msg (TimestampedMessage time (Cmd _ cmd)) -> do
      let (a', phi') = case cmd of 
            F -> (500, phi) 
            B -> (-200, phi) 
            L -> (a, -3)
            R -> (a, 3) 
            _ -> (a, phi) 
      become $ receive (px,py) (vx,vy) a' omega phi'  

laser :: Int -> Int -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> ActorIO ServerMessage (ActorRef ServerMessage) 
laser id shooterId pos velo aInit omegaInit phiInit = actor (show id) (receive pos velo aInit omegaInit phiInit) where
  receive (px,py) (vx,vy) a omega phi = \case 
    Msg (TimestampedMessage prevTime Update) -> do 
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Laser (CommonState id p' v' a omega' phi) shooterId)))
      self >>= \s -> scheduleOnce 1000 s (Msg (TimestampedMessage time Update)) 
      become $ receive p' v' a omega' phi

rotate :: (Double, Double) -> Double -> (Double, Double)
rotate (x,y) rad = let cos' = cos rad; sin' = sin rad in (x * cos' - y * sin', x * sin' + y * cos')
    
add :: (Double, Double) -> (Double, Double) -> (Double, Double)
add (x, y) (x', y') = (x+x', y+y')

scalarMult :: (Double, Double) -> Double -> (Double, Double)
scalarMult (x,y) k = (x*k, y*k)
