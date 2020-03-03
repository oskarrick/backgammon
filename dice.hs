module Dice
( dice
, twoDice
, startGame
, calculateMoves
) where

import System.Random
import System.IO



dice :: IO ()
dice = do
    number <- randomRIO (1,6) :: IO Int
    putStrLn (show number)

twoDice :: IO ()
twoDice = do
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then
        putStrLn (show number ++ " and " ++ show number2 ++ " Double!")
        else putStrLn (show number ++ " and " ++ show number2)

startGame :: IO ()
startGame = do
    number1 <- randomRIO (1,6) :: IO Int
    putStrLn "Player 1, enter any button to roll the dice"
    _ <- getLine
    putStrLn (show number1)
    number2 <- randomRIO (1,6) :: IO Int
    putStrLn "Player 2, enter any button to roll the dice"
    _ <- getLine
    putStrLn (show number2)
    if number1 > number2 then putStrLn ("Player 1 starts!")
    else if number2 > number1 then putStrLn ("Player 2 starts!")
    else startGame

calculateMoves = do
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then return [number, number, number2, number2]
        else return [number,number2]


chooseDice = do
    dices <- calculateMoves
    putStrLn "Choose a dice"
    print dices
    dice <- readLn
    if number == number2 then
        putStrLn (show number ++ ", " ++ show number ++ ", " ++ show number2 ++ ", " ++ show number2)
        else putStrLn (show number ++ ", " ++ show number2)
    do
        dice <- readLn
        if (dice :: Int) == number then print number
        else if (dice :: Int) == number2 then print number2
        else putStrLn "Choose a dice that exists!"
