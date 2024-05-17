module Day2 (
    solve
) where

import Data.Char ( digitToInt, isNumber ) 
import Data.List ( isPrefixOf )
--Day 2 of Advent of Code

--custom dropWhile to remove the char and the space after
dropWhile'               :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []          =  []
dropWhile' p (x:y:xs)
            | p x       =  dropWhile' p (y:xs)
            | otherwise =  xs

turnToArray :: String -> [String]
turnToArray = lines

removeGame :: [String] -> [String]
removeGame [] = []
removeGame ls = fmap (dropWhile' (/=':')) ls

checkColor :: String -> Int
checkColor ls
        |null ls = 0
        |"red" `isPrefixOf` ls = 12
        |"blue" `isPrefixOf` ls = 14
        |"green" `isPrefixOf` ls = 13

checkString :: String -> Bool
checkString "" = True
checkString str = checkNumbers str
    where checkNumbers :: String -> Bool
          checkNumbers "" = True
          checkNumbers (x:xs) = if isNumber x
                                then if isNumber (head xs)
                                     then read [x,head xs] <= checkColor ((tail . tail) xs) && checkString (tail xs)
                                     else digitToInt x <= checkColor (tail xs) && checkString xs
                                else checkString xs

checkGame :: [String] -> [Bool]
checkGame = map checkString

sumTrues :: [Bool] -> Int
sumTrues [] = 0
sumTrues ls = go 1 ls
    where go _ [] = 0
          go n (x:xs) = if x then n + go (n+1) xs else go (n+1) xs

--red green blue  
checkStringB :: String -> [Int]
checkStringB "" = [0,0,0]
checkStringB str = checkNumbers [0,0,0] str
    where   checkNumbers :: [Int] -> String -> [Int]
            checkNumbers ls "" = ls
            checkNumbers (r:g:b:cs) (x:xs) =
                if isNumber x
                then if isNumber (head xs)
                     then checkNumbers (updateInts (read [x, head xs]) (r:g:b:cs) ((tail . tail) xs)) ((tail . tail) xs)
                     else checkNumbers (updateInts (digitToInt x) (r:g:b:cs) (tail xs)) (tail  xs)
                else checkNumbers (r:g:b:cs) xs

checkGameB :: [String] -> [[Int]]
checkGameB = map checkStringB

sumPartB :: [[Int]] -> Int
sumPartB [] = 0
sumPartB ls = sum (fmap product ls)

updateInts :: Int -> [Int] -> String -> [Int]
updateInts _ xs "" = xs
updateInts n (a:b:c:cs) ls
  | "red" `isPrefixOf` ls = if n > a then n:b:c:cs else a:b:c:cs
  | "green" `isPrefixOf` ls = if n > b then a:n:c:cs else a:b:c:cs
  | "blue" `isPrefixOf` ls = if n > c then a:b:n:cs else a:b:c:cs
  | otherwise = a:b:c:cs

solve :: IO ()
solve = do
    putStrLn "Part A Answer: "
    readFile "Day2.txt" >>=  print . sumTrues . checkGame . removeGame . turnToArray
    putStrLn "Part B Answer: "
    readFile "Day2.txt" >>=  print . sumPartB . checkGameB . removeGame . turnToArray