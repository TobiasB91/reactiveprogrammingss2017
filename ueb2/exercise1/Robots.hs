module Robots where

import Control.Concurrent
import System.Random
import Future

wait_ :: Future a-> IO ()
wait_ f= do { wait f; return ()}

data Robot = Robot {id :: Int, pos :: Int, battery :: Int}

instance Show Robot where
  show (Robot i p b) = "Robot #"++ show i++ " at "++ show p++ " [battery: "++ show b++ "]"


move :: Int-> Robot-> IO (Promise Robot)
move max r = do p<- promise
                n<- randomRIO (0, max)
                forkIO (do {r'<- mv r n; complete p r'})
                return p 
           where  mv :: Robot-> Int-> IO Robot
                  mv r n | n <= 0 = return r
                         | otherwise =  do 
                               threadDelay(1000000)
                               mv r{pos= pos r+ 1, battery= battery r- 1} (n-1)

ex :: IO ()
ex = do
  let swarm = [Robot i 0 10 | i<- [1..6]]
  ps <- mapM (move 10) swarm
  mapM_ (\p-> forkIO (wait_ $ future p `onComplete` (putStrLn. show))) ps
  putStrLn "Started moving robots..."

main = ex
