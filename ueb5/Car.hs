module Car where

import Prelude hiding ((<*))
import Shape(Shape(Polygon))
import Region(Region)
import Picture(Picture)
import Animation(picToGraphic)
import SOE(Color)
import Fal

import Debug.Trace(trace)

trans (tx,ty) (x,y) = (x+tx, y+ty)

-- Rotate a point by an angle around the origin
rot :: Float-> (Float, Float)-> (Float, Float)
rot w (x, y)= (x* cos w- y* sin w, x* sin w+ y* cos w)

-- Convert polar coordinates to cartesian and vice versa
cart :: Float-> Float-> (Float, Float)
cart r w = rot w (r, 0) -- to cart
polar :: (Float, Float)-> (Float, Float)
polar (x, y) = (sqrt (x*x+ y*y), atan2 y x) --from cart

-- Draw a polygon
poly :: Behavior [(Float, Float)]-> Behavior Region
poly pts = shape $ lift1 Polygon pts

-- Key press events to start/stop acceleration and breaking
startAcc, stopAB, startBrk :: Event ()
startAcc = keyPress 'a' 
stopAB   = keyPress 's'
startBrk = keyPress 'd'


acceleration :: Behavior Float
acceleration = 0 `untilB` ca where
   ca = (startAcc ->> 0.5 `untilB` ca) .|.
        (stopAB ->> 0 `untilB` ca) .|.
        (startBrk ->> (-0.5) `untilB` ca) -- .|.

rect :: Float -> Float -> Behavior [(Float, Float)]
rect s2 s1 = 
  let s12 = s1/2
      s22 = s2/2
  in constB [(-s12,-s22),(-s12,s22), (s12,s22),(s12,-s22)]
 

motionVec :: Behavior (Float, Float) 
motionVec = lift2 cart velo orientation

position :: Behavior (Float, Float)
position =  vecIntegral motionVec

steerAngle :: Behavior Float
steerAngle = negate $ lift3 clamp (-0.8) 0.8 (fst mouse) where 
  clamp mn mx = max mn . min mx

workAngleR :: Behavior Float
workAngleR = 0 `untilB` ev where
  ev = (when (steerAngle <* 0) `snapshot_` (negate $ atan (0.3 / (radius-0.1))) =>> \x -> (constB x `untilB` ev)) .|.
       (when (steerAngle >* 0) `snapshot_` (atan (0.3 / (radius+0.1))) =>> \x -> (constB x `untilB` ev))

workAngleL :: Behavior Float
workAngleL = 0 `untilB` ev where
  ev = (when (steerAngle >* 0) `snapshot_` (atan (0.3 / (radius-0.1))) =>> \x -> (constB x `untilB` ev)) .|.
       (when (steerAngle <* 0) `snapshot_` (negate $ atan (0.3 / (radius+0.1))) =>> \x -> (constB x `untilB` ev))

velo :: Behavior Float
velo = integral acceleration

orientation :: Behavior Float
orientation = integral $ velo * ( steerAngle / 0.3)

radius :: Behavior Float
radius =  0.3 / (tan $ abs steerAngle)

carBody :: Behavior Picture
carBody = paint green $ move position $ poly $ lift2 map (lift1 rot orientation) $ rect 0.2 0.5 

wheels :: Behavior Picture
wheels = frontWheels `over` rearWheels

frontWheels :: Behavior Picture
frontWheels = frontWheelL `over` frontWheelR

rearWheels :: Behavior Picture
rearWheels = rearWheel (-0.15,0.115) `over` rearWheel (-0.15,-0.115) 

frontWheelL :: Behavior Picture
frontWheelL = paint white $ move position $ poly $ lift2 map (lift1 rot orientation) $ lift2 map (lift1 trans (constB (0.15,0.115))) $ lift2 map (lift1 rot workAngleL) $ rect 0.03 0.1

frontWheelR :: Behavior Picture
frontWheelR = paint white $ move position $ poly $ lift2 map (lift1 rot orientation) $ lift2 map (lift1 trans (constB (0.15,-0.115))) $ lift2 map (lift1 rot workAngleR) $ rect 0.03 0.1

rearWheel :: (Float, Float) -> Behavior Picture
rearWheel p = paint white $ move position $ wheelShape where
  wheelShape = poly $ lift2 map (lift1 rot orientation) $ lift2 map (lift1 trans (constB p)) $ rect 0.03 0.1 

main :: IO ()
main = reactimate "Car" $ lift1 picToGraphic (wheels `over` carBody)
