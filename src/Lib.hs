module Lib
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- limit
multiples3and5 :: Integer -> Integer
multiples3and5 x = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1 .. (x - 1)]

-- limit
evenFib :: Integer -> Integer
evenFib x = sum $ takeWhile (<= x) $ filter even fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs) -- how and why does this work?
    -- https://stackoverflow.com/questions/37243102/haskell-infinite-recursion
