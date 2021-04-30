module Main where

import Lib
import System.Random

check :: String -> String -> Char -> (Bool, String)
check word display c
 = (c `elem`word, [if x==c
        then c
        else y | (x,y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n = 
  do if n==0
       then putStrLn "You lose"
       else if word==display
               then do putStrLn "\nThe correct word is:"
                       putStrLn word 
                       putStrLn "You win!"
               else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n =
  do putStrLn (display ++ "  " ++ take n (repeat '*'))
     putStrLn "  Enter your guess: "
     q <- getLine
     let (correct, display') = check word display (q!!0)
     let n' = if correct then n else n-1
     turn word display' n'

starman :: String -> Int -> IO ()
starman word n = turn word ['-' | x <- word] n

wordList :: [String]
wordList = ["alpha", "beta", "kappa", "delta", "theta"]

main :: IO ()
main = do
    randomNum <- randomRIO (0, length wordList - 1) :: IO Int
    starman (wordList !! randomNum) 5
  

  

