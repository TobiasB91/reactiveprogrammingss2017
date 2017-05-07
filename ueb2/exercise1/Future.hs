module Future where

import Control.Concurrent.MVar 

data Promise a = Promise (Future a)  
data Future a = Future { var :: MVar a,  handle :: Maybe (a -> IO ()) }

promise :: IO (Promise a) 
promise = do 
  var <- newEmptyMVar
  return . Promise $ Future var Nothing

complete :: Promise a -> a -> IO ()
complete (Promise f) = putMVar (var f)

future :: Promise a -> Future a
future (Promise f) = f

onComplete :: Future a -> (a -> IO ()) -> Future a
onComplete f action = f {handle = Just action}

wait :: Future a -> IO a
wait f = do
  a <- takeMVar $ var f 
  case handle f of
    Just action -> action a
    _           -> return ()
  return a 
