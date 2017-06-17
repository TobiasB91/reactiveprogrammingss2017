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
import Data.Maybe
import qualified Data.Map.Lazy as Map 

universe :: ActorIO ServerMessage (ActorRef ServerMessage)
universe = actor "universe" $ initialize 0 where
  initialize idCount = \case 
    Msg (TimestampedMessage t Init) -> do
      num <- liftIO $ randomRIO (8,20)
      asteroids <- forM [idCount..idCount+num] $ \index -> do 
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
      become $ receive (idCount + length asteroids) Map.empty (Map.fromList $ zip [idCount..idCount+num] asteroids) Map.empty Map.empty Map.empty 
      forM_ asteroids (! (Msg (TimestampedMessage t Update)))
  receive idCount connections asteroids spaceships lasers zorgs= \case
    Connect out res -> do
      self >>= \s -> do 
        connection <- connection idCount s    
        forward connection (Connect out res)
        become $ receive (idCount+1) (Map.insert idCount connection connections) asteroids spaceships lasers zorgs
    Disconnect conId-> do
      sender >>= \con -> do 
        let connections' = Map.delete conId connections 
        become $ receive idCount connections' asteroids spaceships lasers zorgs 
    destroy@(Msg (TimestampedMessage t (Destroy id))) -> do
      let new = map (Map.delete id) [asteroids, spaceships, lasers, zorgs]
      forM_ connections (! destroy)
      become $ receive idCount connections (new !! 0) (new !! 1) (new !! 2) (new !! 3)
    msg@(Msg (TimestampedMessage t (SetLifes id lifes))) -> do
      let con = Map.lookup id connections
      when (isJust con) $ fromJust con ! msg
      become $ receive idCount connections asteroids spaceships lasers zorgs
    Msg (TimestampedMessage t (ClientId cId)) -> 
      case Map.lookup cId connections of
        Just connection -> do
          sp <- spaceship cId (0,0) (0,0) 0 0 0 
          num <- liftIO $ randomRIO (2,5) 
          newZorgs <- forM [idCount..idCount+num] $ \index -> do
            [xR,yR] <- liftIO . replicateM 2 $ randomRIO (0,1.0) 
            let pos = (500 - xR * 1000, 500 - yR * 1000) 
            let velo = (0,0)
            let a = 0
            let omega = 0
            let phi = 0
            zorg index pos velo a omega phi sp 
          (liftIO $ timestamped (ClientId cId)) >>= \msg -> connection ! Msg msg
          (liftIO $ timestamped Update) >>= \msg -> (sp ! Msg msg)
          forM_ newZorgs $ \z -> (liftIO $ timestamped Update) >>= \msg -> (z ! Msg msg)
          forM_ newZorgs $ \z -> (liftIO $ timestamped Seek) >>= \msg -> scheduleOnce 1500 z $ Msg msg
          let zorgs' = Map.union (Map.fromList $ zip [idCount..idCount+num] newZorgs) zorgs
          become $ receive (idCount+num+1) connections asteroids (Map.insert cId sp spaceships) 
            lasers zorgs' 
        Nothing -> do
          liftIO $ print "Unexpected: connection not avaible" 
          become $ receive idCount connections asteroids spaceships lasers zorgs
    update@(Msg (TimestampedMessage t (Asteroid _ _ _))) -> do  
      become $ receive idCount connections asteroids spaceships lasers zorgs
      forM_ zorgs (! update)
      forM_ spaceships (! update)
      forM_ connections (! update) 
    update@(Msg (TimestampedMessage t (Zorg _))) -> do  
      become $ receive idCount connections asteroids spaceships lasers zorgs
      forM_ connections (! update) 
    Msg (TimestampedMessage t (CreateAsteroid pos size color)) -> do
      rnds <- liftIO . replicateM 4 $ randomRIO (0,1.0) 
      let omega = (rnds !! 0) * pi * 2 
      let phi = (rnds !! 1) - 0.5 
      let velo = (50 - (rnds !! 2) * 100, 50 - (rnds !! 3) * 100) 
      asteroid <- asteroid idCount pos velo 0 omega phi size color  
      let asteroids' = Map.insert idCount asteroid asteroids
      asteroid ! Msg (TimestampedMessage t Update)
      become $ receive (idCount+1) connections asteroids' spaceships lasers zorgs
    update@(Msg (TimestampedMessage t (Spaceship _))) -> do 
      become $ receive idCount connections asteroids spaceships lasers zorgs
      forM_ connections (! update)
    update@(Msg (TimestampedMessage t (Laser st sId))) -> do 
      become $ receive idCount connections asteroids spaceships lasers zorgs
      forM_ asteroids (! update) 
      forM_ spaceships (! update)
      forM_ zorgs (! update)
      forM_ connections (! update)
    msg@(Msg (TimestampedMessage t (Cmd sId cmd))) -> do
      case Map.lookup sId spaceships of 
        Just sp -> sp ! msg
        _ -> liftIO $ print "spaceship is dead" 
      become $ receive idCount connections asteroids spaceships lasers zorgs 
    msg@(Msg (TimestampedMessage t (CreateLaser sId pos v@(vx,vy) omega))) -> do
      let velo' = add v $ scalarMult (rotate (0,1) omega) 1000 
      let a = 0
      let phi = 0
      laser <- laser idCount sId pos velo' a omega phi 
      laser ! Msg (TimestampedMessage t Update)
      become $ receive (idCount+1) connections asteroids spaceships (Map.insert idCount laser lasers) zorgs


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
      self >>= \s -> scheduleOnce 300 s (Msg (TimestampedMessage time Update))
      become $ receive p' v' omega'
    Msg (TimestampedMessage t (Laser st sId)) -> do
      if (intersects (px,py) (sizeToRadius size) $ pos st) then do
        when (size /= Tiny) $ do
          let size' = case size of 
                Small -> Tiny 
                Medium -> Small
                Big -> Medium
          numAst <- liftIO $ randomRIO (2,4)
          replicateM_ numAst $ do
            parent >>= (! Msg (TimestampedMessage t $ CreateAsteroid (px,py) size' color))
        parent >>= (! Msg (TimestampedMessage t $ Destroy id))
        stop
      else 
        become $ receive (px,py) (vx,vy) omega 

          

spaceship :: Int -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> ActorIO ServerMessage (ActorRef ServerMessage)
spaceship id p@(px,py) v@(vx,vy) aInit omega phiInit = actor (show id) (receive p v aInit omega phiInit 3 0) where
 receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime = \case 
    Msg (TimestampedMessage prevTime Update) -> do  
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Spaceship (CommonState id p' v' a omega' phi))))
      self >>= \s -> scheduleOnce 100 s (Msg (TimestampedMessage time Update))
      become $ receive p' v' aInit omega' phiInit lifes lastCollisionTime
    Msg (TimestampedMessage time (Cmd _ S)) -> do
      parent >>= (! Msg (TimestampedMessage time (CreateLaser id (px,py) (vx,vy) omega)))
      become $ receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime
    Msg (TimestampedMessage _ (Cmd _ cmd)) -> do
      let (a', phi') = case cmd of 
            F -> (500, phi) 
            B -> (-200, phi) 
            L -> (a, -3)
            R -> (a, 3) 
            _ -> (a, phi) 
      become $ receive (px,py) (vx,vy) a' omega phi' lifes lastCollisionTime  
    Msg (TimestampedMessage t (Asteroid st size _)) -> do
      now <- liftIO $ currentTimeMillis
      if now - lastCollisionTime >= 3000 then
        if intersectsC (px,py) spaceshipSize (pos st) (sizeToRadius size) then do 
          parent >>= (! Msg (TimestampedMessage now (SetLifes id $ lifes - 1)))
          become $ receive (px,py) (vx,vy) a omega phi (lifes - 1) now
          when (lifes == 1) $ do
            parent >>= (! Msg (TimestampedMessage now (Destroy id)))
            stop
        else 
          become $ receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime
      else 
        become $ receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime 
    Msg (TimestampedMessage t Seek) -> do
      sender >>= (! Msg (TimestampedMessage t $ Spaceship (CommonState id (px,py) (vx,vy) a omega phi)))
      become $ receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime
    Msg (TimestampedMessage t (Laser st sId)) -> do
      if (intersects (px,py) spaceshipSize $ pos st) && sId /= id then do
        parent >>= (! Msg (TimestampedMessage t (SetLifes id $ lifes - 1)))
        become $ receive (px,py) (vx,vy) a omega phi (lifes - 1) t
        when (lifes == 1) $ do
          parent >>= (! Msg (TimestampedMessage t (Destroy id)))
          stop
      else 
        become $ receive (px,py) (vx,vy) a omega phi lifes lastCollisionTime 



laser :: Int -> Int -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> ActorIO ServerMessage (ActorRef ServerMessage) 
laser id shooterId pos velo aInit omegaInit phiInit = do 
  time <- liftIO $ currentTimeMillis
  actor (show id) (receive pos velo aInit omegaInit phiInit time) where
    receive (px,py) (vx,vy) a omega phi t = \case 
      Msg (TimestampedMessage prevTime Update) -> do 
        if prevTime - t > 1000 then do
          msg <- liftIO $ timestamped $ Destroy id
          parent >>= (! Msg msg)
          stop 
        else do
          time <- liftIO $ currentTimeMillis 
          let deltaS = (fromIntegral $ time - prevTime) / 1000 
          let p' = (px+vx*deltaS, py+vy*deltaS)
          let v' = (vx - deltaS * sin omega * a, vy + deltaS * cos omega * a)
          let omega' = omega + deltaS * phi
          parent >>= (! (Msg (TimestampedMessage time $ Laser (CommonState id p' v' a omega' phi) shooterId)))
          self >>= \s -> scheduleOnce 100 s (Msg (TimestampedMessage time Update)) 
          become $ receive p' v' a omega' phi t


zorg :: Int -> (Double, Double) -> (Double, Double) -> Double -> Double -> Double -> 
  ActorRef ServerMessage -> ActorIO ServerMessage (ActorRef ServerMessage) 
zorg zId pInit vInit aInit omegaInit phiInit sp = actor (show zId) (receive pInit vInit aInit omegaInit phiInit) where
  receive p@(px,py) v@(vx,vy) a omega phi = \case 
    Msg (TimestampedMessage prevTime Update) -> do  
      time <- liftIO $ currentTimeMillis 
      let deltaS = (fromIntegral $ time - prevTime) / 1000 
      let p' = (px+vx*deltaS, py+vy*deltaS)
      let v' = (min 300 (vx - deltaS * sin omega * a), min 300 (vy + deltaS * cos omega * a))
      let omega' = omega + deltaS * phi
      parent >>= (! (Msg (TimestampedMessage time $ Zorg (CommonState zId p' v' a omega' phi))))
      self >>= \s -> scheduleOnce 100 s (Msg (TimestampedMessage time Update))
      prob <- liftIO $ randomRIO (0, 1.0) 
      when (prob <= (0.05 :: Double)) $ parent >>= (! Msg (TimestampedMessage time (CreateLaser zId p' v' omega')))
      become $ receive p' v' aInit omega' phiInit
    msg@(Msg (TimestampedMessage t Seek)) -> do
      sp ! msg
      become $ receive p v a omega phi 
    Msg (TimestampedMessage t (Spaceship st)) -> do
      let distance = add (pos st) (-px, -py)
      let normalizedDistance = unit distance
      let phi' = (((atan2 (snd distance) (fst distance)) -(1/2*pi)) - (omega)) * 3
      let a' = (((dist (pos st) p) - 100) -200) / 1
      time <- liftIO $ currentTimeMillis
      self >>= \s -> scheduleOnce 250 s (Msg (TimestampedMessage time Seek))
      become $ receive p v a' omega phi'
    Msg (TimestampedMessage t (Laser st sId)) -> do
      if (intersects (px,py) spaceshipSize $ pos st) && sId /= zId then do
        parent >>= (! Msg (TimestampedMessage t (Destroy zId)))
        stop
      else 
        become $ receive p v a omega phi  
    Msg (TimestampedMessage t (Asteroid st size _)) -> do
      if intersectsC (px,py) spaceshipSize (pos st) (sizeToRadius size) then do 
        parent >>= (! Msg (TimestampedMessage t (Destroy zId)))
        stop
      else 
        become $ receive p v a omega phi 

rotate :: (Double, Double) -> Double -> (Double, Double)
rotate (x,y) rad = let cos' = cos rad; sin' = sin rad in (x * cos' - y * sin', x * sin' + y * cos')
    
add :: (Double, Double) -> (Double, Double) -> (Double, Double)
add (x, y) (x', y') = (x+x', y+y')

scalarMult :: (Double, Double) -> Double -> (Double, Double)
scalarMult (x,y) k = (x*k, y*k)

divide :: (Double, Double) -> Double -> (Double, Double)
divide _ 0 = (0,0)
divide (x,y) k = (x/k, y/k)

unit :: (Double, Double) -> (Double, Double)
unit p = p `divide` sizeVec p

dist :: (Double, Double) -> (Double, Double) -> Double
dist (px,py) (px', py') = sqrt $ (px-px')^2 + (py-py')^2 

intersects :: (Double, Double) -> Double -> (Double, Double) -> Bool
intersects c r p = dist c p <= r  

intersectsC :: (Double, Double) -> Double -> (Double, Double) -> Double -> Bool
intersectsC p r p' r' = dist p p' < r + r' 

sizeVec :: (Double, Double) -> Double
sizeVec (x,y) = sqrt $ x^2 + y^2

sizeToRadius :: ASize -> Double
sizeToRadius Tiny = 16 
sizeToRadius Small = 27
sizeToRadius Medium = 44
sizeToRadius Big = 95

spaceshipSize :: Double
spaceshipSize = 60
