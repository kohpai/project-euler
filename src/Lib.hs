module Lib
  ( someFunc,
  )
where

import Data.Char (digitToInt)
import Data.Function.Memoize (memoize2)
import Data.List (find, transpose)
import Data.List.Ordered (minus, unionAll)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Universe.Helpers (diagonals)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- limit
multiples3and5 :: Integer -> Integer
multiples3and5 x = sum [a | a <- [1 .. (x - 1)], a `mod` 3 == 0 || a `mod` 5 == 0]

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

primes = 2 : 3 : minus [5, 7 ..] (unionAll [[p * p, p * p + 2 * p ..] | p <- tail primes])

productList' :: String -> Int
productList' str = product (map digitToInt str)

productList :: String -> String -> [Int]
productList [] _ = [0]
productList window [] = [productList' window]
productList window (x : xs) = productList' window : productList (rest ++ [x]) xs
  where
    (_ : rest) = window

largestProduct :: String -> Int -> Int
largestProduct n digits = maximum $ productList (take digits n) (drop digits n)

pythagoreanTriples = [(a, b, c) | c <- [5 ..], b <- [4 .. c -1], a <- [3 .. b -1], (a * a) + (b * b) == (c * c)]

productListNum :: [Int] -> [Int] -> [Int]
productListNum [] _ = [0]
productListNum window [] = [product window]
productListNum window (x : xs) = product window : productListNum (tail window ++ [x]) xs

largestProductNum :: Int -> [Int] -> Int
largestProductNum digits n = maximum $ productListNum (take digits n) (drop digits n)

genStrips :: Int -> [Int] -> [[[Int]]]
genStrips dim nums =
  let rows = chunksOf dim nums
   in let cols = transpose rows
       in let diags1 = diagonals rows
           in let diags2 = diagonals $ map reverse rows
               in [rows, cols, diags1, diags2]

--maximum $ concatMap (map (largestProductNum 4)) $ genStrips 20 [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08, 49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00, 81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65, 52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91, 22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80, 24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50, 32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70, 67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21, 24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72, 21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95, 78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92, 16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57, 86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58, 19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40, 04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66, 88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69, 04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36, 20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16, 20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54, 01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]

trianglePairs :: [(Int, Int)]
trianglePairs = zip triangleNums $ map (length . factors) triangleNums
  where
    triangleNums = 1 : [len + (triangleNums !! (len - 2)) | len <- [2 ..]]
    factors x = factors' x 1 []
    factors' x count [] = factors' x (count + 1) [x, 1]
    factors' x count result
      | count >= head result = result
      | otherwise =
        let (p, r) = x `divMod` count
         in if r == 0
              then factors' x (count + 1) $ p : count : result
              else factors' x (count + 1) result

--find ((>500) . snd) trianglePairs

collatzSeq :: Int -> [Int]
collatzSeq 1 = [1]
collatzSeq x = x : collatzSeq (if even x then x `div` 2 else 3 * x + 1)

--maximumBy (\x y -> compare (snd x) (snd y)) $ map (\x -> (x, length $ collatzSeq x)) [999999, 999998 .. 2]

latticePaths :: Int -> Int -> Int
latticePaths = memoize2 lp
--latticePaths' x y = map (\x' -> map (lp x') [0 ..]) [0 ..] !! x !! y
  where
    lp 0 _ = 1
    lp _ 0 = 1
    lp r d
      | r == d = 2 * lp (r - 1) d
      | otherwise = latticePaths (r -1) d + latticePaths r (d -1)
