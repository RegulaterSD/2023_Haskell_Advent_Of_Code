module Day3 (
    solve
) where

import Data.Char ( isNumber, intToDigit, isDigit ) 
import Data.Maybe ( fromJust )
import Data.List ( elemIndex, group, sort ) 

--Day 3 Advent

{-==================================Part A=====================================-}

getAllNumbers :: [String] -> [Int]
getAllNumbers [] = []
getAllNumbers (x:y:z:ls) = go (symbolPos y) x y z ++ if null ls then [] else getAllNumbers (y:z:ls)
    where go :: [Int] -> String -> String -> String ->  [Int]
          go [] _ _ _ = []
          go (n:ns) x y z = addSurrounding [x, y, z] n : go ns x y z

addSurrounding :: [String] -> Int -> Int
addSurrounding [] _ = 0
addSurrounding (x:y:z:ls) n = sum $ surrounding x y z
    where
        surrounding :: String -> String -> String -> [Int]
        surrounding x y z = map read (removeDuplicates (removeItem "" (makeSurrounding x y z n))) :: [Int]

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) 
    | x == y = removeItem x ys
    | otherwise = y : removeItem x ys

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

makeSurrounding :: String -> String -> String -> Int -> [String]
makeSurrounding [] _ _ n = []
makeSurrounding _ [] _ n = []
makeSurrounding _ _ [] n = []
makeSurrounding a b c n = [getNumbers (n-1) a, getNumbers n a, getNumbers (n+1) a,getNumbers (n-1) b
                        , getNumbers (n+1) b, getNumbers (n-1) c, getNumbers n c, getNumbers (n+1) c]

getNumbers :: Int -> String -> String
getNumbers _ "" = ""
getNumbers n ls = go (getFirstNumber n ls) ls
    where   go :: Int -> String -> String
            go x ls = if isDigit (ls!!x) then (ls!!x) : if (x + 1) > (length ls - 1) then "" else go (x + 1) ls else ""

getFirstNumber :: Int -> String -> Int
getFirstNumber _ "" = 0
getFirstNumber _ "." = 0
getFirstNumber n ls = x
    where x = if isDigit (ls!!(n-1)) && (ls!!n) /= '.'
              then if n > 1 then getFirstNumber (n-1) ls else n-1
              else n

symbolPos :: String -> [Int]
symbolPos [] = []
symbolPos ls = stringArrayToIntArray (getSymbols ls)

stringArrayToIntArray :: [String] -> [Int]
stringArrayToIntArray ls = map read ls :: [Int]

getSymbols :: String -> [String]
getSymbols "" = []
getSymbols ls = removeItem "" (go ls ls)
    where go "" ls = [""]
          go (x:xs) ls = if (x /= '.') && not (isNumber x)
                         then (intToDigit' . fromJust) (elemIndex x ls) : go xs (modifyString (fromJust (elemIndex x ls)) ls)
                         else go xs ls

modifyString :: Int -> String -> String
modifyString _ "" = ""
modifyString n ls =  x ++ '.' : ys
    where (x,_:ys) = splitAt n ls

intToDigit' :: Int -> String
intToDigit' 0 = "0"
intToDigit' n
  | n < 10 = [intToDigit n]
  | (n >= 10) && (n < 100) = [intToDigit (n `div` 10), intToDigit (n `mod` 10)]
  | (n >= 100) && (n < 999) = [intToDigit (n `div` 100), intToDigit ((n `mod` 100) `div` 10), intToDigit (n `mod` 10)]
  | otherwise = ""

turnToArray :: String -> [String]
turnToArray = lines

{- ==================================Part B===================================== -}

partB :: [[Int]] -> Int
partB [] = 0
partB ls = sum $ map product $ filter (\x -> length x == 2) ls

getAllNumbersB :: [String] -> [[Int]]
getAllNumbersB [] = []
getAllNumbersB (x:y:z:ls) = go (symbolPosB y) x y z ++ if null ls then [] else getAllNumbersB (y:z:ls)
    where go :: [Int] -> String -> String -> String -> [[Int]]
          go [] _ _ _ = []
          go (n:ns) x y z = getSurroundingB [x, y, z] n : go ns x y z

getSurroundingB :: [String] -> Int -> [Int]
getSurroundingB [] _ = []
getSurroundingB (x:y:z:ls) n = surrounding x y z
    where
        surrounding :: String -> String -> String -> [Int]
        surrounding x y z = map read (removeDuplicates (removeItem "" (makeSurrounding x y z n))) :: [Int]

symbolPosB :: String -> [Int]
symbolPosB [] = []
symbolPosB ls = stringArrayToIntArray (getStars ls)

getStars :: String -> [String]
getStars "" = []
getStars ls = removeItem "" (go ls ls)
    where go "" ls = [""]
          go (x:xs) ls = if x == '*'
                         then (intToDigit' . fromJust) (elemIndex x ls) : go xs (modifyString (fromJust (elemIndex x ls)) ls)
                         else go xs ls

solve :: IO ()
solve = do
    putStrLn "Part A Answer: "
    readFile "Day3.txt" >>=  print . sum . getAllNumbers . turnToArray
    putStrLn "Part B Answer: "
    readFile "Day3.txt" >>=  print . partB . getAllNumbersB . turnToArray