module GuessNumber where

import Lib
import System.Random


check :: Int -> Int -> [Int] -> [Int] -> (Bool, [Int], [Int])
check input number lessThan greaterThan
 = (input == number,
    if input > number then input:lessThan else lessThan, 
    if input < number then input:greaterThan else greaterThan) 



turn :: Int -> Int -> [Int] -> [Int] -> IO ()
turn number tries lessThan greaterThan = 
  do if tries==0
       then putStrLn ("You lose. The correct number was " ++ show number)
       else mkguess number tries lessThan greaterThan



mkguess :: Int -> Int -> [Int] -> [Int] -> IO ()
mkguess number tries lessThan greaterThan  =
  do 
     putStrLn "Enter your guess: "
     q <- getLine
     let input = read q :: Int
     let (correct, lessThan', greaterThan') = check input number lessThan greaterThan
     if correct 
       then putStrLn "You win"
       else do
              let tries' = if correct then tries else tries-1
              putStrLn "\n\nWrong!!"
              putStrLn ("The secret number is:\nLess than: " ++ show lessThan' ++ " -- " ++ "Greater than: " ++ show greaterThan')
              putStrLn ("\nTries left: " ++ show tries')
              turn number tries' lessThan' greaterThan'


play :: IO ()
play = do
    randomNum <- randomRIO (1, 10) :: IO Int
    putStrLn "--------------------------"
    putStrLn "\nWelcome to Guess a Number"
    putStrLn "Instructions:"
    putStrLn "\tYou have to guess a random number between 1 and 10"
    putStrLn "\tYou only have 5 tries"
    turn randomNum 5 [] []
  

  

