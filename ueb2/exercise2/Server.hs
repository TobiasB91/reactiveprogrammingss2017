{-# LANGUAGE TupleSections #-}

module Server where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import Network
import System.IO
import System.Random.Shuffle 
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import Data.Function
import Data.IORef

type Id = String
type Msg = (Id, String)

data Player = Player { name :: String, score :: Int, hand :: [Int] } deriving Show
data Table = Table { rows :: [[Int]] } deriving Show
data ST = ST { players :: [Player], table :: Table } deriving Show

instance Eq Player where (Player n _ _) == (Player m _ _) = m == n 

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
  writeChan outChan ("", "Starting Game Round.")
  initPlayers <- initialPlayers var
  st <- dealOut initPlayers
  varST <- newIORef st 
  fix $ \loop -> do 
    st' <- readIORef varST
    shuffled <- dealOut (players st') 
    writeIORef varST shuffled
    st' <- readIORef varST 
    unless (any (>=66) $ (map score . players) st') $ do 
      writeChan outChan ("", "========================================")
      fix $ \loop' -> do
        st' <- readIORef varST 
        unless (all null $ (map hand . players) st') $ do
          printHands outChan st'
          newState <- selectCards st' inChan outChan
          writeIORef varST newState
          loop'
      roundState <- readIORef varST
      printScores outChan roundState
      loop
  endState <- readIORef varST 
  let winner = minimumBy (compare `on` score) $ players endState
  writeChan outChan ("", "The winner is " ++ name winner ++ "! Congratulations!")

printScores :: Chan Msg -> ST -> IO ()
printScores out st = forM_ (players st) $ printScore out

printScore :: Chan Msg -> Player -> IO () 
printScore out p = writeChan out (name p, "Your current score is " ++ (show . score) p ++ ".")

printHands :: Chan Msg -> ST -> IO ()
printHands out st = forM_ (players st) $ printHand out

printHand :: Chan Msg -> Player -> IO () 
printHand out p = do
  writeChan out (name p, "Your hand: ")
  writeChan out (name p, unwords . map show $ hand p)

printTable :: Chan Msg -> ST -> IO ()
printTable out st = forM_ (players st) $ printTableForPlayer out (table st) 

printTableForPlayer :: Chan Msg -> Table -> Player -> IO ()
printTableForPlayer out table p = do
  writeChan out (name p, "The current Table: ")
  forM_ (rows table) $ printRow out p

printRow :: Chan Msg -> Player -> [Int] -> IO ()
printRow out p row = writeChan out (name p, unwords . map show $ row)

selectCards :: ST -> Chan Msg -> Chan Msg -> IO ST
selectCards st inChan outChan = do 
  printTable outChan st
  writeChan outChan ("", "Please pick your card.")
  var <- newIORef [] -- map between players and chosen cards
  fix $ \loop -> do
    ps <- readIORef var 
    unless (length ps == length (players st)) $ do 
      (n,msg) <- readChan inChan
      msg' <- try $ readIO msg :: IO (Either SomeException Int)
      case msg' of
        Right choice -> do
          let [p] = filter (==Player n undefined undefined) $ players st 
          if p `elem` map fst ps then 
            writeChan outChan (n, "You picked your card already.") 
          else if choice `notElem` hand p then 
            writeChan outChan (n, "You do not possess this card. Try again")
          else do
             writeIORef var $ (p {hand = delete choice (hand p) }, choice) : ps 
             writeChan outChan (n, "You chose " ++ show choice ++ ". Waiting for other players now.")
        Left _     -> writeChan outChan (n, "Cannot read your input. Try again.") 
      loop
  choices <- readIORef var
  newState <- sortIn st inChan outChan choices 
  writeChan outChan ("", "========================================")
  writeChan outChan (""," ") 
  return newState

sortIn :: ST -> Chan Msg -> Chan Msg -> [(Player, Int)] -> IO ST
sortIn st inChan outChan chosenCards = do
  let cardSeq = sortBy (compare `on` snd) chosenCards
  varST <- newIORef st
  varP <- newIORef []
  forM_ cardSeq $ \t@(p,card) -> do
    st' <- readIORef varST 
    writeIORef varST $ st' {table = Table $ sortBy (compare `on` last) (rows . table $ st')}
    st' <- readIORef varST
    let minColumn = head . map last . rows . table $ st'
    if card < minColumn
    then do 
      newState <- selectRow st' inChan outChan t
      let [p'] = filter (==p) $ players newState 
      ps <- readIORef varP 
      writeIORef varP $ p':ps 
      writeIORef varST newState
    else do 
      let newRows = insertIntoRows (reverse . rows . table $ st') card 
      let (bigRow, rest) = partition ((>5) . length) newRows 
      ps <- readIORef varP 
      if null bigRow -- no row is full 
      then do 
        writeIORef varP $ p:ps
        writeIORef varST $ st' {table = Table newRows }
      else do
        writeIORef varP $ p { score = score p + cardsToScore (init $ head bigRow) } : ps
        writeIORef varST $ st' {table = Table $ [last . head $ bigRow] : rest } 
  newPlayers <- readIORef varP
  st' <- readIORef varST
  writeIORef varST $ st' {players = newPlayers}
  newState <- readIORef varST
  printTable outChan newState 
  return newState

insertIntoRows :: [[Int]] -> Int -> [[Int]]
insertIntoRows [] _ = [] 
insertIntoRows (x:xs) e 
  | last x < e = (x ++ [e]) : xs
  | otherwise  = x : insertIntoRows xs e

cardsToScore :: [Int] -> Int
cardsToScore = sum . map cardToScore 

cardToScore :: Int -> Int
cardToScore x = fiveOrTen + doublet where 
  doublet = let x' = show x 
    in if all (==head x') x' && length x' > 1 then 5 else 0 
  fiveOrTen 
    | x `mod` 10 == 0 = 3 
    | x `mod` 5 == 0 = 2 
    | otherwise = 0 

selectRow :: ST -> Chan Msg -> Chan Msg -> (Player, Int) -> IO ST
selectRow st inChan outChan (p,card) = do
  writeChan outChan (name p, "The card you played does not fit in any row. Please select a row (1-4) to pick up.")
  printTableForPlayer outChan (table st) p
  fix $ \loop -> do
    (n,msg) <- readChan inChan
    if n == name p then do 
      msg' <- try $ readIO msg :: IO (Either SomeException Int)
      case msg' of 
        Right choice -> 
          if choice `elem` [1..4] 
          then do
            let (front, back) = splitAt (choice-1) (rows . table $ st) 
            let selectedRow = head back
            let newPlayers = p {score = score p + cardsToScore selectedRow} : filter (/=p) (players st)
            let newRows = [card] : front ++ tail back
            return $ st {players = newPlayers, table = Table newRows} 
          else do 
            writeChan outChan (name p, "This row does not exist. Try again.")
            loop 
        Left _       -> do 
          writeChan outChan (name p, "Cannot read your input. Try again.") 
          loop
    else 
      loop 

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
