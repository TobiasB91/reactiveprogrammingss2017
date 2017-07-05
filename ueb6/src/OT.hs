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

data TextAction = Retain Int
                | Insert String
                | Delete Int deriving Show

newtype TextOperation = TextOperation [TextAction] deriving Show

instance Operation TextOperation where
  compose (TextOperation a) (TextOperation b) = TextOperation $ compose' a b where
    compose' [] [] = []
    compose' (Delete n: as) bs   = Delete n : compose' as bs
    compose' as (Insert cs : bs) = Insert cs : compose' as bs
    compose' (Retain n : as) (Retain n' : bs) 
      | n > n'  = Retain n' : compose' (Retain (n-n') : as) bs
      | n < n'  = Retain n : compose' as (Retain (n'-n) : bs)
      | otherwise = Retain n : compose' as bs
    compose' (Retain n : as) (Delete n' : bs) 
      | n > n'  = Delete n' : compose' (Retain (n-n') : as) bs
      | n < n'  = Delete n : compose' as (Retain (n'-n) : bs)
      | otherwise = Delete n : compose' as bs
    compose' (Insert cs : as) (Retain n : bs) 
      | n > length cs = Insert cs : compose' as (Retain (n-length cs) : bs)
      | n < length cs = Insert (take n cs) : compose' (Insert (drop n cs) : as) bs
      | otherwise     = Insert cs : compose' as bs
    compose' (Insert cs : as) (Delete n :bs)  
      | n > length cs = compose' as $ Delete (n - length cs) : bs 
      | n < length cs = compose' ((Insert $ drop n cs) : as) bs
      | otherwise     = compose' as bs
  transform (TextOperation a) (TextOperation b) = pair $ transform' a b where
    pair (as',bs') = (TextOperation as', TextOperation bs')
    transform' [] [] = ([],[])
    transform' (Insert cs : as) bs = case transform' as bs of (as',bs') -> (Insert cs : as', Retain (length cs) : bs')
    transform' as (Insert cs : bs) = case transform' as bs of (as',bs') -> (Retain (length cs) : as', Insert cs : bs')
    transform' (Retain n: as) (Retain n': bs) 
      | n > n' = case transform' (Retain (n-n') : as) bs of (as',bs') -> (Retain n' : as', Retain n' : bs')
      | n < n' = case transform' as (Retain (n'-n) : bs) of (as',bs') -> (Retain n : as', Retain n : bs')  
      | otherwise = case transform' as bs of (as',bs') -> (Retain n: as', Retain n': bs')
    transform' (Delete n : as) (Delete n' : bs)
      | n > n' = transform' (Delete (n-n') : as) bs
      | n < n' = transform' as $ Delete (n'-n) : bs 
      | otherwise = transform' as bs
    transform' (Retain n : as) (Delete n' : bs) 
      | n > n' = case transform' (Retain (n-n') : as) bs of (as',bs') -> (as', Delete n' : bs') 
      | n < n' = case transform' as (Delete (n'-n) : bs) of (as',bs') -> (as', Delete n : bs') 
      | otherwise = case transform' as bs of (as',bs') -> (as', Delete n : bs')
    transform' (Delete n : as) (Retain n' : bs) 
      | n > n' = case transform' (Delete (n-n') : as) bs of (as',bs') -> (Delete n' : as', bs')
      | n < n' = case transform' as (Retain (n'-n) : bs) of (as',bs') -> (Delete n : as', bs')
      | otherwise = case transform' as bs of (as',bs') -> (Delete n: as', bs')

instance Document String TextOperation where
  noop d 
    | length d == 0 = TextOperation [] 
    | otherwise = TextOperation [Retain $ length d]
  applyOp d (TextOperation as) = apply' as d where
    apply' [] [] = []
    apply' (Retain 0 : as) cs = apply' as cs
    apply' (Retain n : as) (c:cs) = c : apply' (Retain (n-1) : as) cs
    apply' (Insert cs : as) d = cs ++ apply' as d
    apply' (Delete 0 : as) cs = apply' as cs
    apply' (Delete n : as) (_:cs) = apply' (Delete (n-1) : as) cs

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
transformCursor (TextOperation op) c = foldr transf c $ split op c where 
  transf op c = case op of
    Retain n -> c 
    Insert str -> c + length str
    Delete n -> c - n
  split [] _ = []
  split (x:xs) c 
    | c <= 0 = [] 
    | otherwise = case x of
        Retain n -> 
          if n <= c 
          then x : split xs (c-n) 
          else [Retain c]
        Delete n ->
          if n <= c
          then x : split xs (c-n)
          else [Delete c]
        Insert cs -> 
          if length cs <= c 
          then x : split xs (c - length cs)
          else [Insert $ take c cs]

serverState :: Server o d -> d
serverState (Server start ops) = foldl applyOp start ops

serverStateOp :: Server o d -> o
serverStateOp (Server start ops) = foldl compose (noop start) ops

serverRevision :: Server o d -> Int
serverRevision (Server start ops) = length ops

appendOperation :: Server o d -> o -> Int -> (o,Server o d)
appendOperation (Server start ops) op rev = (op', Server start (ops ++ [op'])) where
  op' = foldl (\a b -> fst $ transform a b) op $ drop rev ops
