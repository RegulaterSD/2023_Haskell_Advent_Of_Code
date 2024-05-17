module Day4 (
    solve
) where

import Data.Char (isNumber, intToDigit, digitToInt)

{-==================================Part A=====================================-}

getFinalNumber :: [Int] -> [Int]
getFinalNumber [] = []
getFinalNumber (x:xs) = if x == 0 then 0 : getFinalNumber xs else (2^(x-1)) : getFinalNumber xs

checkAllGames :: [[Int]] -> [Int]
checkAllGames [] = []
checkAllGames (x:y:xs) = checkGame x y 0 : checkAllGames xs
    where checkGame :: [Int] -> [Int] -> Int -> Int
          checkGame _ [] n = n
          checkGame [] _ n = n
          checkGame (x:xs) ys n = if x `elem` ys then checkGame xs ys (n+1) else checkGame xs ys n

splitAll :: [[String]] -> [[Int]]
splitAll = concatMap (map splitNumbers)
    where splitNumbers :: String -> [Int]
          splitNumbers [] = []
          splitNumbers (x:y:z:xs) = read [x,y] : splitNumbers xs

addSpaceToAll :: [String] -> [String]
addSpaceToAll [] = []
addSpaceToAll ls = map addSpace ls
    where addSpace :: String -> String
          addSpace [] = []
          addSpace ls = ls ++ " "

splitArray :: [String] -> [[String]]
splitArray = map (\ x -> [takeWhile' (/= '|') x, dropWhile' (/= '|') x])

removeGame :: [String] -> [String]
removeGame = map (dropWhile' (/= ':'))

--custom dropWhile to remove the char and the space after
dropWhile'               :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []          =  []
dropWhile' p (x:y:xs)
            | p x       =  dropWhile' p (y:xs)
            | otherwise =  xs

--custom takeWHile to remove the space before the symbol
takeWhile'               :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []          =  []
takeWhile' p (x:y:xs)
            | p y       =  x : takeWhile' p (y:xs)
            | otherwise =  []

turnToArray :: String -> [String]
turnToArray = lines

partA :: [String] -> [Int]
partA [] = []
partA ls = (getFinalNumber . checkAllGames . splitAll . fmap addSpaceToAll . splitArray . removeGame) ls


{-==================================Part B=====================================-}

make1Array :: Int -> [Int]
make1Array 0 = []
make1Array n = replicate n 1

totalTickets :: [Int] -> [Int]
totalTickets [] = []
totalTickets ls = addWinning ls (make1Array (length ls))

addWinning :: [Int] -> [Int] -> [Int]
addWinning [] _ = []
addWinning (x:xs) (y:ys) = y : addWinning xs (go ys x y) 
    where go [] _ _ = []
          go ls 0 _ = ls
          go (y:ys) x n = (y + n) : go ys (x-1) n

partB :: [String] -> [Int]
partB [] = []
partB ls = (totalTickets . checkAllGames . splitAll . fmap addSpaceToAll . splitArray . removeGame) ls

solve :: IO ()
solve = do
    putStrLn "Part A Answer: "
    readFile "Day4.txt" >>=  print . sum . partA . turnToArray
    putStrLn "Part B Answer: "
    readFile "Day4.txt" >>=  print . sum . partB . turnToArray