module Fal where

import Prelude hiding ((<*))
import SOE hiding (Region, Event, Color(..))
import qualified SOE as G (Region, Event)
import Animation (picToGraphic)
import Shape
import Picture
import Memo1
import Draw (xWin,yWin,intToFloat)
-- import Word (word32ToInt)
import Control.Concurrent.Chan
import Debug.Trace(trace)

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
infixr 3 &&*
infixr 2 ||*

type Time = Float
type UserAction = G.Event









newtype Behavior a 
  = Behavior (([Maybe UserAction],[Time]) -> [a])
newtype Event a 
  = Event (([Maybe UserAction],[Time]) -> [Maybe a])

time :: Behavior Time
time = Behavior (\(_,ts) -> ts)

constB :: a -> Behavior a
constB x = Behavior (\_ -> repeat x)

($*) :: Behavior (a->b) -> Behavior a -> Behavior b
Behavior ff $* Behavior fb
  = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

lift0 :: a -> Behavior a
lift0 = constB

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f b1 = lift0 f $* b1

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2 = lift1 f b1 $* b2

lift3 :: (a -> b -> c -> d) -> 
         (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 f b1 b2 b3 = lift2 f b1 b2 $* b3

pairB :: Behavior a -> Behavior b -> Behavior (a,b)
pairB = lift2 (,)

fstB :: Behavior (a,b) -> Behavior a
fstB  = lift1 fst

sndB :: Behavior (a,b) -> Behavior b
sndB  = lift1 snd

paint :: Behavior Color -> Behavior Region -> Behavior Picture
paint = lift2 Region

red, blue, yellow, green, white, black :: Behavior Color
red    = lift0 Red
blue   = lift0 Blue
yellow = lift0 Yellow 
green  = lift0 Green
white  = lift0 White
black  = lift0 Black

shape :: Behavior Shape -> Behavior Region
shape   = lift1 Shape

ell, rec :: Behavior Float -> Behavior Float -> Behavior Region
ell x y = shape (lift2 Ellipse   x y) 
rec x y = shape (lift2 Rectangle x y)

translate :: (Behavior Float, Behavior Float)-> Behavior Region -> Behavior Region
translate (Behavior fx, Behavior fy) (Behavior fp)
      = Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
          where aux x y p = Translate (x,y) p

move :: Behavior (Float, Float)-> Behavior Region -> Behavior Region
move (Behavior fxy) (Behavior fp)
      = Behavior (\uts -> zipWith Translate (fxy uts) (fp uts))


(>*),(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

(&&*),(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)
(||*) = lift2 (||)

over :: Behavior Picture -> Behavior Picture -> Behavior Picture
over = lift2 Over

instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational

instance Num a => Num (Behavior a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0 . fromInteger

instance Show (Behavior a)  where
  showsPrec n a s = "<< Behavior >>"

--instance Eq (Behavior a) where
--  a1 == a2 = error "Can't compare behaviors."

instance Floating a => Floating (Behavior a) where
  pi    = lift0 pi
  sqrt  = lift1 sqrt
  exp   = lift1 exp
  log   = lift1 log
  sin   = lift1 sin
  cos   = lift1 cos
  tan   = lift1 tan
  asin  = lift1 asin
  acos  = lift1 acos
  atan  = lift1 atan
  sinh  = lift1 sinh
  cosh  = lift1 cosh
  tanh  = lift1 tanh
  asinh = lift1 asinh
  acosh = lift1 acosh
  atanh = lift1 atanh

untilB :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `untilB` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) =
            b : case e of 
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> fb' (us,ts)

memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)

switch :: Behavior a -> Event (Behavior a) -> Behavior a
Behavior fb `switch` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) ~(b:bs) = 
            b : case e of 
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> loop us ts es (fb' (us,ts))

lbp :: Event ()
lbp = Event (\(uas,_) -> map getlbp uas)
      where getlbp (Just (Button _ True True)) = Just ()
            getlbp _                           = Nothing

(=>>) :: Event a -> (a->b) -> Event b
Event fe =>> f = Event (map (fmap f) . fe)
(->>) :: Event a-> b-> Event b
e ->> v = e =>> \_ -> v

while :: Behavior Bool -> Event ()
while (Behavior fb) 
  = Event (\uts -> map aux (fb uts))
    where aux True  = Just ()
          aux False = Nothing

unique :: (Show a, Eq a) => Event a -> Event a
unique (Event fe) =
      Event (\uts -> aux (fe uts))
      where aux xs = zipWith remdup (Nothing:xs) xs
            remdup x y | x==y      = Nothing
                       | otherwise = y

when :: Behavior Bool -> Event ()
when = unique . while

{-
integral :: Num a=> Behavior a-> Behavior a
integral (Behavior fb)
  = Behavior (\uts@(us,t:ts) -> 0 : loop t 0 ts (fb uts))
      where loop t0 acc (t1:ts) (a:as) 
                 = let acc' = acc + (t1-t0)*a
                   in acc' : loop t1 acc' ts as
-}

-- This would need not be so complicated if tuples of Num a would be instances
-- of Num, i.e. instance Num a, Num b=> Num (a, b) but alas...
genIntegral :: a-> (a-> a-> a)-> (Float-> a-> a)-> Behavior a-> Behavior a
genIntegral zero plus mult (Behavior fb)
  = Behavior (\uts@(us,t:ts) -> zero : loop t zero ts (fb uts))
      where loop t0 acc (t1:ts) (a:as) 
                 = let acc' = acc `plus` ((t1-t0) `mult` a)
                   in acc' : loop t1 acc' ts as

integral :: Behavior Float-> Behavior Float
integral = genIntegral 0 (+) (*)

-- vector integral (2-dimensional)
vecIntegral :: Behavior (Float, Float)-> Behavior (Float, Float)
vecIntegral = genIntegral (0, 0)
                          (\(x1, y1) (x2, y2)-> (x1+ x2, y1+ y2))
                          (\t (x, y)-> (t* x, t* y))


-- Debugging: tracing a behavior
traceBeh :: Show a=> String-> Behavior a-> Behavior a
traceBeh tag (Behavior fb) = 
  Behavior (\uts-> map (\a-> trace (tag++ show a) a) $ fb uts)

withElem  :: Event a -> [b] -> Event (a,b)
withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
  where loop (Just a  : evs) (b:bs) = Just (a,b) : loop evs bs
        loop (Nothing : evs)    bs  = Nothing    : loop evs bs

withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = e `withElem` bs =>> snd

(.|.) :: Event a -> Event a -> Event a
Event fe1 .|. Event fe2 
  = Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
      where aux Nothing  Nothing  = Nothing
            aux (Just x) _        = Just x
            aux _        (Just y) = Just y

keyEvent :: (Char-> Maybe b)-> Event b
keyEvent getkey =
   Event (\(uas,_) -> map gk uas)
   where gk (Just (Key ch True)) = getkey ch 
         gk _                    = Nothing

keyPress :: Char-> Event ()
keyPress ch = keyEvent (\c-> if c == ch then Just () else Nothing)

key :: Event Char
key = keyEvent Just

snapshot :: Event a -> Behavior b -> Event (a,b)
Event fe `snapshot` Behavior fb
  = Event (\uts -> zipWith' aux (fe uts) (fb uts))
      where aux (Just x) y = Just (x, y)
            aux Nothing  _ = Nothing

zipWith' f ~(x:xs) ~(y:ys) = f x y : zipWith' f xs ys

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd

step :: a -> Event a -> Behavior a
a `step` e = constB a `switch` e =>> constB

stepAccum :: a -> Event (a->a) -> Behavior a
a `stepAccum` e = b 
   where b = a `step` (e `snapshot` b =>> uncurry ($))


instance Functor Behavior where
  fmap f (Behavior fb) = Behavior (fmap f. fb)


mm :: Event Coordinate
mm = Event (\(uas,_) -> map getmm uas)
     where getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
           getmm _                     = Nothing
 
gPtToPt :: (Int, Int) -> Coordinate
gPtToPt (x,y) = ( pixelToInch (x - 300)
                , pixelToInch (250 - y) )
 
pixelToInch  :: Int -> Float
pixelToInch n = intToFloat n / 100

mouse :: (Behavior Float, Behavior Float)
mouse = (fstB m, sndB m)
          where m = (0,0) `step` mm

reactimate :: String -> Behavior Graphic -> IO ()
reactimate title franProg
  = runGraphics $
    do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
              drawBufferedGraphic
       (us,ts,addEvents) <- windowUser w
       addEvents
       let drawPic (Just g) = 
             do setGraphic w g
                quit <- addEvents
                if quit 
                  then return True
                  else return False
           drawPic Nothing  = return False
       let Event fe = sample `snapshot_` franProg
       run drawPic (fe (us,ts))
       closeWindow w
  where
    run f (x:xs) = do
      quit <- f x
      if quit
        then return ()
        else run f xs
    run f [] = return ()
 
sample :: Event ()
sample = Event (\(us,_) -> map aux us)
  where aux Nothing  = Just ()
        aux (Just _) = Nothing

windowUser :: Window -> IO ([Maybe UserAction], [Time], IO Bool)
windowUser w
  = do (evs, addEv) <- makeStream
       t0 <- timeGetTime
       let addEvents =
             let loop rt = do
                   mev <- maybeGetWindowEvent w
                   case mev of
                     Nothing -> return False
                     Just e  -> case e of
                        Key ' ' True -> return True
                        Closed -> return True
                        _ -> addEv (rt, Just e) >> loop rt
             in do t <- timeGetTime
                   let rt = w32ToTime (t-t0)
                   quit <- loop rt
                   addEv (rt, Nothing)
                   return quit
       return (map snd evs, map fst evs, addEvents)

w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000

makeStream :: IO ([a], a -> IO ())
makeStream = do
  ch <- newChan
  contents <- getChanContents ch
  return (contents, writeChan ch)
