module FalOut where

import Prelude hiding ((<*))
import SOE hiding (Region, Event, Color(..))
import qualified SOE as G (Region, Event)
import Animation (picToGraphic)
import Shape
import Picture

-- This is the code not need in FAL, but used for exposition of the design
-- decisions.

newtype Behavior1 a 
  = Behavior1 ([(UserAction,Time)] -> Time -> a)

inList :: [Int] -> Int -> Bool
inList xs y = elem y xs

manyInList' :: [Int]-> [Int]-> [Bool]
manyInList' xs ys = map (inList xs) ys

xs = [2,4,6,8,10] :: [Int]
ys = [3,6,9]      :: [Int]

result2 :: [Bool]
result2 = manyInList xs ys
 
manyInList :: [Int] -> [Int] -> [Bool]
manyInList [] _ = []
manyInList _ [] = []
manyInList (x:xs) (y:ys) =
  if x<y then          manyInList xs (y:ys)
         else (x==y) : manyInList (x:xs) ys

newtype Behavior2 a 
  = Behavior2 ([(UserAction,Time)] -> [Time] -> [a])
newtype Behavior3 a 
  = Behavior3 ([UserAction] -> [Time] -> [a])
newtype Behavior4 a 
  = Behavior4 ([Maybe UserAction] -> [Time] -> [a])
