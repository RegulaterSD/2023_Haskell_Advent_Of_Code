--Day 1 of Advent of Code

module Day1 (
    solve
) where

import Data.Char ( isNumber )
import Data.List ( isPrefixOf )


getNumbers :: String -> String
getNumbers [] = []
getNumbers (x:xs) = if isNumber x then x : getNumbers xs else getNumbers xs

wordToNumber :: String -> String
wordToNumber ls
        |null ls = []
        |"one" `isPrefixOf` ls = "1" ++ wordToNumber (tail ls)
        |"two" `isPrefixOf` ls = "2" ++ wordToNumber (tail ls)
        |"three" `isPrefixOf` ls = "3" ++ wordToNumber (tail ls)
        |"four" `isPrefixOf` ls = "4" ++ wordToNumber (tail ls)
        |"five" `isPrefixOf` ls = "5" ++ wordToNumber (tail ls)
        |"six" `isPrefixOf` ls = "6" ++ wordToNumber (tail ls)
        |"seven" `isPrefixOf` ls = "7" ++ wordToNumber (tail ls)
        |"eight" `isPrefixOf` ls = "8" ++ wordToNumber (tail ls)
        |"nine" `isPrefixOf` ls = "9" ++ wordToNumber (tail ls)
        |isNumber (head ls) = head ls : wordToNumber (tail ls)
        |otherwise = wordToNumber (tail ls)

digits :: String -> Int
digits [] = 0
digits ls = read [head ls, last ls]

makeArray :: [String] -> [Int]
makeArray [] = []
makeArray ls = fmap (digits . getNumbers) ls

makeArrayPartB :: [String] -> [Int]
makeArrayPartB [] = []
makeArrayPartB ls = fmap (digits . wordToNumber) ls

turnToArray :: String -> [String]
turnToArray = lines

solve :: IO ()
solve = do
    putStrLn "Part A Answer: "
    readFile "Day1.txt" >>=  print . sum . makeArray . turnToArray
    putStrLn "Part B Answer: "
    readFile "Day1.txt" >>=  print . sum . makeArrayPartB . turnToArray