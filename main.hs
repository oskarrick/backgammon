

import System.Random
import System.IO
import Data.List

type Position = Int
type Move = Int
type AmountCheckers = Int
type Board = [Triangle]
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers deriving (Eq, Show)
data Checkers = Black | White deriving (Show,Eq)

{-
functionIdentifier arguments
A brief human-readable description of the purpose of the function.
PRE:  ... precondition on the arguments, if any ...
RETURNS: ... description of the result, in terms of the arguments ...
SIDE EFFECTS: ... side effects, if any, including exceptions ...
EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}

{-
main
The initial menu when running the file allowing the user to start the game or quit
RETURNS: When given the input "2", returns an empty tuple which terminates the process and effectively exits the game
SIDE EFFECTS:
EXAMPLES:
-}
main :: IO ()
main = do
    putStrLn ("MENU")
    putStrLn ("1. Start game")
    putStrLn ("2. Quit")
    choice <- getLine
    if choice == "1" then startGame
    else if choice == "2" then return ()
    else main

{-
startGame
Decides whether black or white starts the game depending on who rolls the highest number dice
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}
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
        start newGameState White moves
    else if number2 > number1 then 
        start newGameState Black moves
    else startGame

{-
start Board Checkers [Int]

PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}
start :: Board -> Checkers -> [Int] -> IO ()
start board checkers moves = do 
    if moves == [] then do 
        newmoves <- calculateMoves
        (if checkers == Black then start board White newmoves
        else start board Black newmoves)
        else do
            printGameState board
            putStrLn ("")
            putStrLn (show checkers ++ "'s turn") 
            putStrLn ("Moves: " ++ show moves)
            --moveChecker checkers moves board

{-
newGameState
Constructs a new game board
RETURNS: A board with all checkers reset to the starting position
EXAMPLES:
-}
newGameState :: Board
newGameState = [Checker Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,Checker White 6 5,
                Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,
                Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,
                Checker Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,Checker White 24 2]

{-
calculateMoves
Calculates the moves for a player
RETURNS: A list of two numbers. If the two numbers are equal then it returns four of the same numbers
EXAMPLES:
-}
calculateMoves :: IO [Int]
calculateMoves = do
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then return [number, number, number2, number2]
        else return [number,number2]

{-
chooseDice [Int]
Chooses which die to use
PRE:
RETURNS: The chosen die
SIDE EFFECTS:
EXAMPLES:
-}
chooseDice :: [Int] -> IO Int
chooseDice moves = do
  putStrLn $ "Choose a dice \n"++show moves
  dice <- getLine
  if read dice `elem` moves
    then return (read dice)
    else chooseDice moves

{-
printGameState board
Shows the current state of the game
PRE:
RETURNS: Prints the current state of the game on the board
SIDE EFFECTS:
EXAMPLES:
-}
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

{-
isCheckerWhite triangle
Checks if a Triangle has white checkers
PRE:
RETURNS: A boolean
EXAMPLES:
-}
isCheckerWhite :: Triangle -> Bool
isCheckerWhite (Checker White _ _) = True
isCheckerWhite _ = False

{-
isCheckerBlack triangle
Checks if a Triangle has black checkers
PRE:
RETURNS: A boolean
EXAMPLES:
-}
isCheckerBlack :: Triangle -> Bool
isCheckerBlack (Checker Black _ _) = True
isCheckerBlack _ = False

{-
amountTri triangle
Counts the amount of checkers in a Triangle
PRE:
RETURNS: An Int representing the amount of checkers in the triangle
SIDE EFFECTS: ... side effects, if any, including exceptions ...
EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
amountTri :: Triangle -> Int
amountTri (Empty _ amount) = amount
amountTri (Checker _ _ amount) = amount

{-
checkerOptions checkers board

PRE:  ... precondition on the arguments, if any ...
RETURNS: ... description of the result, in terms of the arguments ...
SIDE EFFECTS: ... side effects, if any, including exceptions ...
EXAMPLES: ... especially if useful to highlight delicate issues; also consider including counter-examples ...
-}
checkerOptions :: Checkers -> Board -> Board
checkerOptions _ [] = []
checkerOptions a@White (x:xs) = if isCheckerWhite x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs
checkerOptions a@Black (x:xs) = if isCheckerBlack x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs

{-
amountOfCheckers board checkers
Counts the total amount of checkers on the board
PRE:
RETURNS: An Int representing the amount of checkers on the board
SIDE EFFECTS:
EXAMPLES:
-}
amountOfCheckers :: Board -> Checkers -> Int
amountOfCheckers (x:xs) checker = amountOfCheckers' $ checkerOptions checker (x:xs)
                                    where
                                      amountOfCheckers' (x:[]) = amountTri x
                                      amountOfCheckers' (x:xs) = amountOfCheckers' xs + amountTri x

{-
winCheck board checkers [Int]
Checks if either player has won the game
PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}
winCheck :: Board -> Checkers -> [Int] -> IO ()
winCheck board checkers moves = if amountOfCheckers board White == 0 
    then  putStrLn ("White wins!")
    else if amountOfCheckers board Black == 0 
        then putStrLn ("Black wins!")
        else start board checkers moves