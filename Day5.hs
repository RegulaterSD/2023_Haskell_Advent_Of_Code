module Day5 (
    solve
) where

turnToArray :: String -> [String]
turnToArray = lines

solve :: IO ()
solve = do
    putStrLn "Part A Answer: "
    readFile "Day5.txt" >>=  print . turnToArray
    --putStrLn "Part B Answer: "
    --readFile "Day4.txt" >>=  print . turnToArray