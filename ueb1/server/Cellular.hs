module Cellular (runCA, dead) where

import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict as M
import qualified System.Random as R
import Data.Maybe (catMaybes) 
import Control.Monad (unless)

data Status = Alive | Dead deriving (Eq, Show) 
type Cell = (Int, Int) 
type Board = M.Map Cell Status
type CA a = S.StateT Board IO a 
type Rule = (Cell -> Board -> Status)

nearCells :: Cell -> [Cell]
nearCells (x,y) = [(x',y') | x' <- [x-1..x+1], y' <- [y-1..y+1]]

nearCellsStatus :: Cell -> Board -> [Status]
nearCellsStatus c m = catMaybes $ (`M.lookup` m) <$> nearCells c

countNearAliveCells :: Cell -> Board -> Int
countNearAliveCells c = length . filter (==Alive) . nearCellsStatus c

-- Assumption: c is a key within m
rule :: Rule 
rule c m = case M.lookup c m of 
  Just Alive -> if countNearAliveCells c m `elem` [1..5] then Alive else Dead 
  Just Dead  -> if countNearAliveCells c m == 3 then Alive else Dead 
  Nothing    -> error "Unexpected Error" 

stepCellular :: Rule -> CA [(Cell, Status)]
stepCellular r = do
  st <- S.get
  let st' = [(c, r c st) | c <- M.keys st] 
  S.put $ M.fromList st' 
  return st'

convergeCellular :: Rule -> CA ()
convergeCellular r = do
  st <- M.toList <$> S.get
  st' <- stepCellular r
  unless (st == st') $ convergeCellular r 

initialState :: Int -> CA ()
initialState s = do
  n <- S.lift $ R.randomRIO (0,s*s)
  [xs,ys] <- S.lift . S.replicateM 2 . S.replicateM n $ R.randomRIO (0,s)
  let living = M.fromList $ zipWith (\x y -> ((x,y),Alive)) xs ys 
  let rest = M.fromList [((x,y), Dead) | x <- [0..s], y <- [0..s]]
  S.put $ M.union living rest

dead :: Cell -> Board -> Bool
dead c m = case M.lookup c m of
  Just Dead -> True
  _         -> False

runCA :: Int -> IO Board
runCA s = S.execStateT (initialState s >> convergeCellular rule) undefined 
