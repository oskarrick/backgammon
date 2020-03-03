module Dice 
( dice
, twoDice
, startGame
, calculateMoves
, chooseDice
, menu
) where

import System.Random
import System.IO
import Data.List
import Backgammon

type Position = Int
type Move = Int
type AmountCheckers = Int
type Board = [Triangle]
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers deriving (Eq, Show)
data Checkers = Black | White deriving (Show,Eq)



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
    if number1 > number2 then putStrLn ("Player 1 starts as white!")
    else if number2 > number1 then putStrLn ("Player 2 starts as black!")
    else startGame
    if number1 > number2 then start White newGameState
    else start Black newGameState

calculateMoves = do
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then return [number, number, number2, number2]
        else return [number,number2]


chooseDice = do
    putStrLn "Choose a dice"
    print dices
    dice <- readLn
    if (dice :: Int) `elem` dices then return dice
    else chooseDice
