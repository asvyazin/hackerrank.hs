import Control.Monad

hello_worlds n = do
  forM_ [1..n] $ \_ ->
    putStrLn "Hello World"

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
   n <- readLn :: IO Int
   hello_worlds n
