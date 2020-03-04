

import System.Random
import System.IO
import Data.List

type Position = Int
type Move = Int
type AmountCheckers = Int
type Board = [Triangle]
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers deriving (Eq, Show)
data Checkers = Black | White deriving (Show,Eq)

main = do
    putStrLn ("MENU")
    putStrLn ("1. Start game")
    putStrLn ("2. Quit")
    choice <- getLine
    if choice == "1" then startGame
    else if choice == "2" then return ()
    else main

startGame :: IO ()
startGame = do
    number1 <- randomRIO (1,6) :: IO Int
    putStrLn "White, enter any button to roll the dice"
    _ <- getLine
    putStrLn (show number1)
    number2 <- randomRIO (1,6) :: IO Int
    putStrLn "Black, enter any button to roll the dice"
    _ <- getLine
    putStrLn (show number2)
    moves <- calculateMoves
    if number1 > number2 then 
        start White newGameState moves
    else if number2 > number1 then 
        start Black newGameState moves
    else startGame

start color gamestate moves = do 
    if moves == [] then do 
        newmoves <- calculateMoves
        (if color == Black then start White gamestate newmoves
        else start Black gamestate newmoves)
        else do
            printGameState gamestate
            putStrLn ("")
            putStrLn (show color ++ "'s turn") 
            putStrLn ("Moves: " ++ show moves)
            --moveChecker color moves gamestate

newGameState :: Board
newGameState = [Checker Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,Checker White 6 5,
                Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,
                Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,
                Checker Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,Checker White 24 2]

calculateMoves = do
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then return [number, number, number2, number2]
        else return [number,number2]

chooseDice :: [Int] -> IO Int
chooseDice moves = do
  putStrLn $ "Choose a dice \n"++show moves
  dice <- getLine
  if read dice `elem` moves
    then return (read dice)
    else chooseDice moves

{--chooseDice moves = do
    putStrLn "Choose a dice"
    print moves
    dice <- readLn
    if (dice :: Int) `elem` moves then return dice
    else chooseDice --}

printGameState :: Board -> IO ()
printGameState ((Empty position checkers):[]) = do
  putStrLn $ "Empty " ++ show position ++ " " ++ show 0
printGameState ((Empty position checkers):triangles) = do
  putStrLn $ "Empty " ++ show position ++ " " ++ show 0
  printGameState triangles
printGameState ((Checker Black position checkers):[]) = do
  putStrLn $ "Black "++ show position ++ " " ++ show checkers
printGameState ((Checker White position checkers):[]) = do
  putStrLn $ "White "++ show position ++ " " ++ show checkers
printGameState ((Checker White position checkers):triangles) = do
  putStrLn $ "White "++ show position ++ " " ++ show checkers
  printGameState triangles
printGameState ((Checker Black position checkers):triangles) = do
  putStrLn $ "Black "++ show position ++ " " ++ show checkers
  printGameState triangles