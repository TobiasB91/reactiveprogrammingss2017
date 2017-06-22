module FalEx where

import Prelude hiding ((<*))
import SOE hiding (Region, Event, Color(..))
import qualified SOE as G (Region, Event)
import Animation (picToGraphic)
import Shape
import Picture
import Fal


color1 :: Behavior Color
color1 = red `untilB` lbp ->> blue

-- Some test code
uas = cycle [Nothing, Just (Button (0,0) True True), Nothing]
ts  = [1,2 ..] :: [Time]
stream1 = let Behavior fb = color1
          in take 3 (fb (uas,ts))
test beh = reactimate "FAL Test" (lift1 picToGraphic beh)

counter = 0 `stepAccum` lbp ->> (+1)
stream2 = let Behavior fb = counter
          in take 20 (fb (uas,ts))


cball1   = paint color1  circ
cball1r  = paint color1r circ
cball1h  = paint color1h circ
cball2   = paint color2  circ
cball2r  = paint color2r circ
cball2h  = paint color2h circ
cball3   = paint color3  circ
cball4   = paint color4  circ
cball5   = paint color5  circ

circ, pulse :: Behavior Region
circ     = translate (cos time, sin time) (ell 0.2 0.2)
pulse    = ell (cos time * 0.5) (cos time * 0.5)

ball1 :: Behavior Picture
ball1 = paint color1 circ

pulse1 :: Behavior Picture
pulse1 = paint color2 pulse

color1r :: Behavior Color
color1r = red  `untilB` lbp ->>
          blue `untilB` lbp ->>
          color1r

color2r :: Behavior Color
color2r = red `untilB` ce where
          ce = (lbp ->> blue `untilB` ce) .|.  
               (key ->> yellow `untilB` ce)

color2h = red `switch` ((lbp ->> blue) .|. (key ->> yellow))
color5 = red `untilB` when (time >* 5) ->> blue

ball2 :: Behavior Picture
ball2 = paint red (translate (x,y) (ell 0.2 0.2)) where
  g  =  -4
  x  =  -3 + integral 0.5
  y  = 1.5 + integral vy
  vy = integral g `switch`
       (hity `snapshot_` vy =>>  \v'-> lift0 (-v') + integral g)
  hity = when (y <* -1.5)

ball2x :: Behavior Picture
ball2x = paint red (translate (x,y) (ell 0.2 0.2)) where
  g  =  -4
  x  =  -3 + integral vx
  vx =  0.5 `switch` (hitx ->> -vx)
  hitx = when (x <* -3 ||* x >* 3)
  y  = 1.5 + integral vy
  vy = integral g `switch` 
         (hity `snapshot_` vy =>> \v'-> lift0 (-v') + integral g)
  hity = when (y <* -1.5)

pb :: Picture
pb = let Behavior f = paddleball 2
     in f (repeat Nothing, cycle [0.1, 0.2 ..]) !! 3

color1h = red `switch` (lbp `withElem_` cycle [blue,red])

color2 :: Behavior Color
color2 = red `untilB` (lbp ->> blue .|. key ->> yellow)

color3 = white `switch` (key =>> \c ->
           case c of 'R' -> red
                     'B' -> blue
                     'Y' -> yellow 
                     _   -> white  )

color4 = white `switch` (key `snapshot` color4 =>> \(c,old) ->
           case c of 'R' -> red
                     'B' -> blue
                     'Y' -> yellow 
                     _   -> lift0 old)

ball3 = paint color4 circ3
circ3 = translate mouse (ell 0.2 0.2)

paddleball :: Float-> Behavior Picture
paddleball vel = walls `over` paddle `over` pball vel

walls = let upper = paint blue (translate ( 0,1.7) (rec 4.4 0.05))
            left  = paint blue (translate (-2.2,0) (rec 0.05 3.4))
            right = paint blue (translate ( 2.2,0) (rec 0.05 3.4))
        in upper `over` left `over` right

paddle = paint red (translate (fst mouse, -1.7) (rec 0.5 0.05))

pball vel =
 let xvel    = vel `stepAccum` xbounce ->> negate
     xpos    = integral xvel
     xbounce = when (xpos >*  2 ||* xpos <* -2)
     yvel    = vel `stepAccum` ybounce ->> negate
     ypos    = integral yvel
     ybounce = when (ypos >* 1.5 
               ||* ypos      `between` (-2.0,-1.5) &&*
                   fst mouse `between` (xpos-0.25,xpos+0.25))
 in paint yellow (translate (xpos, ypos) (ell 0.2 0.2))

x `between` (a,b) = x >* a &&* x <* b

pong = reactimate "Pong!" $ lift1 picToGraphic (paddleball 2.5)
