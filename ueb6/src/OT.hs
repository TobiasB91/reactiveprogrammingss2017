{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExistentialQuantification #-}

module OT where
import Control.Applicative

class Operation o where
  compose :: o -> o -> o
  transform :: o -> o -> (o,o)

class Operation o => Document d o where
  noop :: d -> o
  applyOp :: d -> o -> d

---

data TextAction = Retain
                | Insert Char
                | Delete deriving Show

newtype TextOperation = TextOperation [TextAction] deriving Show

instance Operation TextOperation where
  compose (TextOperation a) (TextOperation b) = TextOperation $ compose' a b where
    compose' [] [] = []
    compose' (Delete : as) bs            = Delete : compose' as bs
    compose' as (Insert c : bs)          = Insert c : compose' as bs
    compose' (Retain : as) (Retain:bs)   = Retain : compose' as bs
    compose' (Retain : as) (Delete:bs)   = Delete : compose' as bs
    compose' (Insert c : as) (Retain:bs) = Insert c : compose' as bs
    compose' (Insert _ : as) (Delete:bs) = compose' as bs
  transform (TextOperation a) (TextOperation b) = pair $ transform' a b where
    pair (as',bs') = (TextOperation as', TextOperation bs')
    transform' [] [] = ([],[])
    transform' (Insert c : as) bs          = case transform' as bs of (as',bs') -> (Insert c : as', Retain : bs')
    transform' as (Insert c : bs)          = case transform' as bs of (as',bs') -> (Retain : as', Insert c : bs')
    transform' (Retain : as) (Retain : bs) = case transform' as bs of (as',bs') -> (Retain : as', Retain : bs')
    transform' (Delete : as) (Delete : bs) = transform' as bs
    transform' (Retain : as) (Delete : bs) = case transform' as bs of (as',bs') -> (as', Delete : bs')
    transform' (Delete : as) (Retain : bs) = case transform' as bs of (as',bs') -> (Delete : as', bs')

instance Document String TextOperation where
  noop = TextOperation . map (\_ -> Retain)
  applyOp d (TextOperation as) = apply' as d where
    apply' [] [] = []
    apply' (Retain : as) (c:cs) = c : apply' as cs
    apply' (Insert c : as) d = c : apply' as d
    apply' (Delete : as) (_:cs) = apply' as cs

---

data Client o d = (Operation o, Document d o) => Client {
  clientRevision :: Integer,
  pending :: Maybe o,
  buffer :: Maybe o
}

newClient :: (Operation o, Document d o) => Client o d
newClient = Client 0 Nothing Nothing

localEdit :: Client o d -> o -> (Bool, Client o d)
localEdit (Client rev pending buffer) operation = (null pending, Client rev newPending newBuffer) where
    newPending = pending <|> (Just operation)
    newBuffer  = pending *> fmap (`compose` operation) buffer

remoteEdit :: Client o d -> o -> (o, Client o d)
remoteEdit (Client rev pending buffer) o = case (pending,buffer) of
    (Nothing,Nothing) -> (o, Client (rev + 1) Nothing Nothing)
    (Just p,Nothing)  -> let (p',o') = transform p o in
                             (o',Client (rev + 1) (Just p') Nothing)
    (Just p,Just b)   -> let (p',o') = transform p o
                             (b',o'') = transform b o' in
                             (o'',Client (rev + 1) (Just o') (Just b'))

ack :: Client o d -> (Maybe o, Client o d)
ack (Client rev pending buffer) = (buffer, Client (rev + 1) buffer Nothing)

isSynchronized :: Client o d -> Bool
isSynchronized (Client rev pending buffer) = null pending

---

data Server o d = (Operation o, Document d o) => Server {
    initialState :: d,
    history :: [o]
}


type Cursor = Int 

transformCursor :: TextOperation -> Cursor -> Cursor
transformCursor (TextOperation op) c = foldr transf c $ take c op where 
  transf op c = case op of
    Retain -> c 
    Insert _ -> c + 1
    Delete -> c - 1

serverState :: Server o d -> d
serverState (Server start ops) = foldl applyOp start ops

serverStateOp :: Server o d -> o
serverStateOp (Server start ops) = foldl compose (noop start) ops

serverRevision :: Server o d -> Int
serverRevision (Server start ops) = length ops

appendOperation :: Server o d -> o -> Int -> (o,Server o d)
appendOperation (Server start ops) op rev = (op', Server start (ops ++ [op'])) where
  op' = foldl (\a b -> fst $ transform a b) op $ drop rev ops
