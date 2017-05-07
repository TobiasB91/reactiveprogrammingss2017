{-# LANGUAGE TupleSections #-}

module Server where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix
import Network
import System.IO
import System.Random.Shuffle 
import Data.Maybe
import Data.Char
import Data.List.Split

type Id = String
type Msg = (Id, String)

data Player = Player { name :: String, score :: Int, hand :: [Int] } deriving Show
data Table = Table { rows :: [[Int]] } deriving Show
data ST = ST { players :: [Player], table :: Table } deriving Show

startServer :: IO ()
startServer = withSocketsDo $ do 
  putStrLn "Starting server..."
  sock <- listenOn $ PortNumber 8000
  inChan <- newChan
  outChan <- newChan
  players <- newMVar [] 
  forkIO $ newGame inChan outChan players
  handleSocket sock inChan outChan players


newGame :: Chan Msg -> Chan Msg -> MVar [(Id, Bool)] -> IO ()
newGame inChan outChan ps = do 
  preGame inChan outChan ps
  game inChan outChan ps 

preGame :: Chan Msg -> Chan Msg -> MVar [(Id, Bool)] -> IO ()
preGame inChan outChan ps = fix $ \loop -> do
  ps' <- readMVar ps
  unless (all snd ps' && length ps' > 1) $ do
    (id', msg) <- readChan inChan
    let msg' = toLower <$> msg
    when (msg' == "ready") $ do 
      ps' <- takeMVar ps
      let ps'' = map (\e@(n,b) -> if n == id' then (n, True) else e) ps'
      putMVar ps ps''
      unless (ps' == ps'') $ writeChan outChan ("",id' ++ " is ready to play.")
    when (msg' == "help") $  
      writeChan outChan (id', "Available commands: 'ready', 'help', 'whoisready', 'whoisconnected'")
    when (msg' == "whoisready") $ do
      ps' <- readMVar ps 
      forM_ (map fst $ filter snd ps') $ writeChan outChan . (id', ) . (++ " is ready.")
    when (msg' == "whoisonline") $ do
      ps' <- readMVar ps
      forM_ (map fst ps') $ writeChan outChan . (id', ) . (++ " is connected.")
    loop

game :: Chan Msg -> Chan Msg -> MVar [(Id, Bool)] -> IO ()
game inChan outChan var = do
  writeChan outChan ("", "Starting Game Round")
  players <- initialPlayers var
  st <- dealOut players
  return ()

initialPlayers :: MVar [(Id, Bool)] -> IO [Player]
initialPlayers var = do
  ps <- (map fst) <$> readMVar var 
  return $ map (\n -> Player n 0 []) ps

dealOut :: [Player] -> IO ST
dealOut ps = do
  cards <- shuffleCards 
  let ps' = map (\i -> (ps !! i) {hand = cards !! i}) [0..length ps-1]
  let table = Table . map (:[]) $ last cards
  return $ ST ps' table

maxScore :: ST -> Int
maxScore = maximum . map score . players

shuffleCards :: IO [[Int]] 
shuffleCards = chunksOf 10 <$> shuffleM [1..104]  

handleSocket :: Socket -> Chan Msg -> Chan Msg -> MVar [(Id, Bool)] -> IO ()
handleSocket sock inChan outChan ps = forever $ do
  (handle, _, _) <- accept sock
  inChan' <- dupChan inChan
  forkIO $ identify handle inChan' outChan ps

identify :: Handle -> Chan Msg -> Chan Msg -> MVar [(Id, Bool)] -> IO ()
identify handle inChan outChan ps = fix $ \loop -> do
  hPutStrLn handle "Please enter your name"
  msg <- readHandle handle
  ps' <- takeMVar ps
  if null msg && isJust (lookup msg ps') then do
    putMVar ps ps'
    loop
  else if length ps' > 9 || (all snd ps' && length ps' > 1) then do
    putMVar ps ps'
    hPutStrLn handle $ "Sorry, " ++ msg ++ ", but there is currently a game in progress. Try again later." 
  else let id = msg in do
    hPutStrLn handle $ "Welcome " ++ id ++ "!"
    forkIO $ readClient handle inChan id
    outChan' <- dupChan outChan
    forkIO $ writeClient handle outChan' id
    writeChan outChan' ("", id ++ " joined the session.")
    putMVar ps $ (id,False):ps'
    return ()

readClient :: Handle -> Chan Msg -> Id -> IO ()
readClient handle ch id = forever $ do
  msg <- readHandle handle 
  writeChan ch (id, msg)  

readHandle :: Handle -> IO String
readHandle handle = filterMsg <$> hGetLine handle where
  filterMsg = filter (not . flip elem ['\r','\n'])

writeClient :: Handle -> Chan Msg -> Id -> IO ()
writeClient handle ch id = forever $ do
  (id', msg) <- readChan ch
  when (null id' || id==id') $ hPutStrLn handle msg



