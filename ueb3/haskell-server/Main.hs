{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (const) 
import Data.Aeson (encode,decode)
import Data.String
import Data.List 
import Data.Maybe (maybe, fromMaybe)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static
import qualified Network.Wai.Handler.Warp as Warp
import System.Random (newStdGen)
import Control.Exception (try, evaluate, SomeException)
import Control.Monad
import qualified Data.Map.Strict as Map

import Messages
import Parser
import Actors hiding (Failed)


data State = State { getSheet :: ActorRef SheetMessage }

handleSocket :: WS.Connection -> State -> IO ()
handleSocket conn state = do
  let send msg = WS.sendTextData conn (encode msg)
  -- Empfange die Daten vom Client
  msg <- WS.receiveData conn 
  let sheet = getSheet state
  case decode msg of 
    Nothing -> do
      putStrLn $ "invalid message from client: '" ++ show msg ++ "'"
      handleSocket conn state
    Just (FormulaUpdate x y Nothing) -> do 
      NewCell newCell <- ask sheet . CreateCell $ x:show y
      ask newCell $ ParseCell "" 
      ask newCell Evaluate
      send (CellUpdate x y $ Right Nothing)
    Just (FormulaUpdate x y (Just formula)) -> do 
      NewCell newCell <- ask sheet . CreateCell $ x:show y 
      ask newCell $ ParseCell formula 
      ask newCell Evaluate
      return ()
  handleSocket conn state

data SheetMessage = CreateCell String |
  NewCell (ActorRef SheetMessage) | 
  ParseCell String | Evaluate | ReqSubs | 
  Subs [ActorRef SheetMessage] |  
  Result Integer | Build | ReqLookUp String | 
  ResultLookUp (Maybe (ActorRef SheetMessage)) |
  Subscribe (ActorRef SheetMessage) | Subscribed SheetMessage |
  Failed String | Done | Update SheetMessage | 
  Terminate (ActorRef SheetMessage)| Unsubscribe

spreadsheet :: (Message -> IO ()) -> IO (ActorRef SheetMessage)
spreadsheet action = actor Nothing (Behavior $ receive Map.empty) where
  receive cells context = \case 
    CreateCell cord -> do
      let oldCell = Map.lookup cord cells 
      subs <- do 
        case oldCell of 
          Just oc -> do 
            Subs subList <- ask oc ReqSubs
            ask oc $ Terminate oc
            return subList
          Nothing -> return []
      newCell <- cell (self context) cord subs action
      respond context $ NewCell newCell
      become $ receive $ Map.insert cord newCell cells
    ReqLookUp cord -> do 
      respond context $ ResultLookUp $ Map.lookup cord cells 
      become $ receive cells

cell :: ActorRef SheetMessage -> String -> [ActorRef SheetMessage] -> (Message -> IO ()) -> IO (ActorRef SheetMessage)
cell parent cord subs action = actor (Just parent) (Behavior $ initial cord subs action) where
  initial cord subs action context = \case 
    ParseCell input -> do
      let ast' = parseFormula input
      case ast' of 
        Right ast -> do
          if not (isLoop (Cell cord) ast) then do
            newExpr <- expr (self context) ast 
            ask newExpr Build
            respond context Done
            become $ evaluating cord newExpr subs action
          else do 
            newEmptyExpr <- empty $ self context 
            ask newEmptyExpr Build
            respond context $ Failed "contains self reference"
            become $ evaluating cord newEmptyExpr subs action
        Left err -> do
          newEmptyExpr <- empty $ self context
          ask newEmptyExpr Build
          respond context $ Failed $ show err 
          become $ evaluating cord newEmptyExpr subs action
  evaluating cord exprA subs action context = \case
    ReqSubs -> do
      respond context $ Subs subs 
      become $ evaluating cord exprA subs action
    Evaluate -> do
      result <- ask exprA Evaluate
      forM_ subs $ \sub -> send sub (self context) $ Update result   
      respond context result
      let x = head cord 
      let y = read . tail $ cord
      case result of
        Result val -> action $ CellUpdate x y $ Right $ Just val 
        Failed err -> action $ CellUpdate x y $ Left err
      become $ evaluating cord exprA subs action 
    Subscribe newRef -> do 
      outcome <- ask exprA Evaluate
      respond context $ Subscribed outcome
      become $ evaluating cord exprA (newRef:subs) action
    Unsubscribe -> do
      let subs' = delete (sender context) subs 
      become $ evaluating cord exprA subs' action
    Update _ -> do 
      send (self context) (self context) Evaluate
      become $ evaluating cord exprA subs action
    Terminate oc -> do
      ask exprA $ Terminate oc
      stop $ self context 
      respond context Done
      become undefined
    Result _ -> become $ evaluating cord exprA subs action 
    Failed _ -> become $ evaluating cord exprA subs action

expr :: ActorRef SheetMessage -> Expr -> IO (ActorRef SheetMessage) 
expr parent ast = actor (Just parent) (Behavior $ initial ast) where
  initial ast context = \case 
    Build ->  
      case ast of 
        Cell cord -> do
          cell <- cellRef (self context) cord
          ask cell Build
          respond context Done 
          become $ evaluating cell 
        Const val -> do
          const <- const (self context) val 
          ask const Build
          respond context Done 
          become $ evaluating const
        Binary op e1 e2 -> do
          binop <- binary (self context) e1 e2 op 
          ask binop Build
          respond context Done 
          become $ evaluating binop 
  evaluating actor context = \case
    Evaluate -> do 
      result <- ask actor Evaluate
      respond context result 
      become $ evaluating actor
    Update _ -> do 
      send parent (self context) $ Update undefined
      become $ evaluating actor 
    Terminate oc -> do
      ask actor $ Terminate oc
      stop $ self context 
      respond context Done
      become undefined

cellRef :: ActorRef SheetMessage -> String -> IO (ActorRef SheetMessage)
cellRef parent cord = actor (Just parent) (Behavior $ initial cord)  where
  initial cord context = \case 
    Build -> do
      ResultLookUp result <- ask (root parent) $ ReqLookUp cord
      case result of 
        Just cellA -> do
          Subscribed curVal <- ask cellA $ Subscribe $ self context 
          respond context Done 
          become $ evaluating cord curVal 
        _ -> do
          NewCell emptyCell <- ask (root parent) $ CreateCell cord 
          ask emptyCell $ ParseCell ""
          ask emptyCell $ Subscribe $ self context
          respond context $ Failed $ "no value for: " ++ cord
          become $ evaluating cord $ Failed $ "no value for: " ++ cord
  evaluating cord curVal context = \case 
    Evaluate -> do
      respond context curVal
      become $ evaluating cord curVal
    Update newVal -> do
      send parent (self context) $ Update undefined
      become $ evaluating cord newVal
    Terminate oc -> do
      send oc (self context) Unsubscribe 
      stop $ self context
      respond context Done
      become undefined

const :: ActorRef SheetMessage -> Integer -> IO (ActorRef SheetMessage)
const parent val = actor (Just parent) (Behavior $ initial val) where
  initial val context = \case 
    Build -> do
      respond context Done
      become $ evaluating val
  evaluating val context = \case 
    Evaluate -> do
      respond context $ Result val 
      become $ evaluating val
    Terminate _ -> do
      stop $ self context
      respond context Done
      become undefined 

empty :: ActorRef SheetMessage -> IO (ActorRef SheetMessage)
empty parent = actor (Just parent) (Behavior initial) where
  initial context = \case 
    Build -> do
      respond context Done
      become $ evaluating 
  evaluating context = \case 
    Evaluate -> do
      respond context $ Failed "N/A" 
      become $ evaluating
    Terminate _ -> do
      stop $ self context
      respond context Done
      become undefined 

binary :: ActorRef SheetMessage -> Expr -> Expr -> Op -> IO (ActorRef SheetMessage)
binary parent lhs rhs op = actor (Just parent) (Behavior $ initial lhs rhs op) where
  initial lhs rhs op context = \case 
    Build -> do
      exp <- expr (self context) lhs
      exp' <- expr (self context) rhs 
      ask exp Build 
      ask exp' Build 
      respond context Done 
      become $ evaluating exp exp' op
  evaluating l r op context = \case
    Evaluate -> do
      lVal <- ask l Evaluate
      rVal <- ask r Evaluate
      case (lVal, rVal) of 
        (Result lVal, Result rVal) -> do 
          outcome <- try . evaluate $ (app op) lVal rVal 
          case outcome of 
            Right result -> do 
              respond context $ Result result 
              become $ evaluating l r op
            Left err -> do
              respond context $ Failed $ show (err :: SomeException) 
              become $ evaluating l r op
        _ -> do 
          respond context $ Failed "binary failed"
          become $ evaluating l r op 
    Update _ -> do
      send parent (self context) $ Update undefined
      become $ evaluating l r op
    Terminate oc -> do
      ask l $ Terminate oc
      ask r $ Terminate oc
      stop $ self context
      respond context Done
      become undefined

app :: Integral a => Op -> (a -> a -> a)
app Plus = (+)
app Minus = (-)
app Multiply = (*)
app Division = div

isLoop :: Expr -> Expr -> Bool 
isLoop (Cell x) (Cell y) = x == y 
isLoop (Cell x) (Const _) = False
isLoop c@(Cell x) (Binary _ e e') = isLoop c e || isLoop c e'
isLoop _ _= False 

----------------------------------------------------------------------------

-- Die folgenden funktionen dienen dem generellen Setup der Server Anwendung
-- und sind für die Lösung der Aufgabe nicht relvant.

-- Der statische Teil der Web Anwendung (html + javascript)
static = Static.staticApp staticSettings
staticSettings = (Static.defaultFileServerSettings "scala-client/assets")
  -- Caching ausschalten, damit die Anwendung nicht neu gestartet werden
  -- muss, wenn der Scala teil neu Kompiliert wurde.
  { Static.ssMaxAge = Static.NoMaxAge }

-- Der WebSocket
socket pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  let send msg = WS.sendTextData conn (encode msg)
  sheet <- spreadsheet send 
  handleSocket conn $ State sheet

-- Starte die Webanwendung auf Port 3000
main = Warp.run 3000 $
  WS.websocketsOr WS.defaultConnectionOptions socket static
