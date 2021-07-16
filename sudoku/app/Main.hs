module Main where

import Data.List

-- Get each rows
toRows :: [Int] -> [[Int]]
toRows [] = []
toRows b =
  part : toRows rest
    where
      (part, rest) = splitAt 9 b

every :: Int -> [Int] -> [Int]
every n xs = case drop (n-1) xs of
               (y: ys) -> y : every n ys
               [] -> []

-- Get each cols
toCols' :: [Int] -> Int -> [[Int]]
toCols' _ 10 = []
toCols' b x = (last (take x b) : every 9 (drop x b)) : toCols' b (x + 1)

toCols :: [Int] -> [[Int]]
toCols b = toCols' b 1

-- Get each boxes
toBoxes' :: [Int] -> Int -> [[Int]]
toBoxes' _ 9 = []
toBoxes' b x =
  concat (take 3 . drop ((x `mod` 3)*3) <$> (take 3 . drop ((x `div` 3)*3)) (toRows b)) : toBoxes' b (x+1)

toBoxes b = toBoxes' b 0

-- Make sure each rows, cols, and boxes all add up to 45
isDone b =
   and $ and . fmap ((==45) . sum) . ($b) <$> [toCols, toRows, toBoxes]

-- Find possible number on given box
possible b i =
  [1..9] \\ (toCols b !! x ++ toRows b !! y ++ toBoxes b !! ((y `div` 3) * 3 + (x `div` 3)))
    where
      x = i `mod` 9
      y = i `div` 9

-- Solve!
solve' 81 b = b
solve' x b = case (b !! x == 0, isDone b) of
          (_, True) -> b
          (False, False) -> solve' (x+1) b
          (True, _) ->
            if null pos then b else head pos
              where
                  (f, e) = splitAt x b
                  pos = filter isDone (solve' (x+1) . (\x -> f++ x : drop 1 e) <$> possible b x)

solve = solve' 0

main :: IO ()
main = do
  print $ solve [0,7,0,0,0,0,0,5,0,0,0,0,0,0,0,1,8,0,1,0,0,9,0,6,0,0,2,0,0,2,0,7,0,0,0,1,0,0,1,6,0,3,2,0,0,5,0,0,0,9,0,4,6,0,4,0,0,8,0,5,0,0,6,0,5,9,0,0,0,0,0,0,0,6,0,0,0,0,0,4,0]