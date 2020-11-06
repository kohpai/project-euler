module Lib
  ( someFunc,
  )
where

import Data.List (find, sort)
import Data.Maybe (fromMaybe)

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

largestPrime :: Integer -> Integer
largestPrime x = if firstPrime < x then largestPrime $ x `div` firstPrime else x
  where
    firstPrime = (+ 2) $ fromIntegral $ length $ takeWhile (> 0) $ map (x `mod`) [2 ..]

isPalindrome :: Integer -> Bool
isPalindrome x = let str = show x in and $ zipWith (==) str $ reverse str

largestPalindrome' :: Integer -> Integer -> Maybe (Integer, Integer)
largestPalindrome' begin end
  | begin < end = Nothing
  | begin == end =
    let p = begin * end
     in if isPalindrome p then Just (p, begin) else Nothing
  | otherwise =
    case find isPalindrome $ map (* begin) [begin, begin - 1 .. end] of
      Nothing -> largestPalindrome' (begin - 1) end
      Just p ->
        let d = p `div` begin
            (p', d') = fromMaybe (0, 0) $ largestPalindrome' (begin - 1) (d + 1) -- Just to test fromMaybe
         in if p < p' then Just (p', d') else Just (p, d)

largestPalindrome :: Integer -> Maybe (Integer, Integer)
largestPalindrome digits = largestPalindrome' begin end
  where
    begin = (10 ^ digits) - 1
    end = 10 ^ (digits - 1)

sumSquarediff :: Integer -> Integer
sumSquarediff x = (^ 2) (sum xs) - sum (map (^ 2) xs)
  where
    xs = [x, x -1 .. 1]
