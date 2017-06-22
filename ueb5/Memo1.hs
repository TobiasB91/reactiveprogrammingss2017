








  {-# OPTIONS_GHC -fglasgow-exts #-}

  module Memo1 (memo1) where

  import Data.IORef
  import System.IO.Unsafe



  import GHC.Prim
  unsafePtrEq x y = case reallyUnsafePtrEquality# x y of
        1# -> True
        _  -> False



  -- import Hugs.IOExts (unsafePtrEq)

  memo1 :: (a->b) -> (a->b)
  memo1 f = unsafePerformIO $ do
    cache <- newIORef []
    return $ \x -> unsafePerformIO $ do
                vals <- readIORef cache
                case x `inCache` vals of
                  Nothing -> do let y = f x
                                writeIORef cache [(x,y)] -- ((x,y) : 
  --                                if null vals then [] else [head vals])
                                return y
                  Just y  -> do return y

  inCache :: a -> [(a,b)] -> Maybe b
  x `inCache` [] = Nothing
  x `inCache` ((x',y'):xys) =
     if unsafePtrEq x x' then Just y' else x `inCache` xys

  fib1 n = fibstr !! n
    where fibstr = 0 : 1 : zipWith (+) fibstr (tail fibstr)

  fib2 n = fibstr nil !! n
    where fibstr x = 0 : 1 : zipWith (+) (fibstr x) (tail (fibstr x))

  fib3 n = fibstr nil !! n
    where fibstr = memo1 $ \x -> 0 : 1 : zipWith (+) (fibstr x) (tail (fibstr x))
  nil = [] : nil
