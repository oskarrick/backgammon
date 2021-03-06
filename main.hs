import System.Random
import System.IO
import Data.List
import Test.HUnit

{-
Represents the position on the board
The position is an Int
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Position = Int

{-
Represents a move
The move is an Int
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Move = Int

{-
Represents the amount of checkers
The amount is an Int
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type AmountCheckers = Int

{-
Represents a Backgammon board
The Backgammon board is made up of a list of triangles
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
type Board = [Triangle]

{-
Represents a triangle on a Backgammon board
The first value constructor determines whether the triangle is empty or not
If the triangle is not empty the value constructor after that is Checkers which is the color of the checkers in the triangle
After that is Position which declares the triangle's position on the board
Lastly is the amount of checkers in the traingle with AmountCheckers
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers deriving (Eq, Show)

{-
Represents the color of the checkers
Color is either black or white
INVARIANT:  ... a predicate on elements of the datatype that the code preserves at all times ...
-}
data Checkers = Black | White deriving (Show,Eq)

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
    if number1 > number2 then do
      putStrLn ("White starts!\n")
      start White moves newGameState
    else if number2 > number1 then do
      putStrLn ("Black starts!\n")
      start Black moves newGameState
    else startGame

{-
start Board Checkers [Int]

PRE:
RETURNS:
SIDE EFFECTS:
EXAMPLES:
-}
start :: Checkers -> [Int] -> Board -> IO ()
start checkers moves board = do
  if winCheck board checkers
    then if checkers == Black
          then do
            putStrLn ("BLACK WINS!")
            main
          else do
            putStrLn ("WHITE WINS!")
            main
    else do
    if moves == [] then do
        newmoves <- calculateMoves
        (if checkers == Black then start White newmoves board
        else start Black newmoves board)
        else do
            printGameState board
            putStrLn ("")
            putStrLn (show checkers ++ "'s turn")
            putStrLn ("Moves: " ++ show moves ++ "\n")
            moveChecker checkers moves board

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
  putStrLn $ "Choose a die "++show moves
  dice <- getLine
  if dice == ""
    then chooseDice moves
    else if read dice `elem` moves
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

position :: Triangle -> Int
position (Checker _ pos _) = pos
position (Empty pos _) = pos

{-
checkerOptions checkers board
Checks which checkers (for a give color) are on the board and where they are
RETURNS: a board with every Triangle with checkers as color
EXAMPLES:
-}
checkerOptions :: Checkers -> Board -> Board
checkerOptions _ [] = []
checkerOptions a@White (x:xs) = if isCheckerWhite x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs
checkerOptions a@Black (x:xs) = if isCheckerBlack x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs

{- chooseChecker board
   Chooses a checker (Triangle) and returns it
   RETURNS: A Triangle from the Board board
   SIDE EFFECTS: Prints strings and accepts inputs

-}
chooseChecker :: Board -> IO Triangle
chooseChecker chkrs = do
  putStrLn ("Moveable checkers:")
  printGameState chkrs
  putStrLn $ "\nChoose a checker (1-" ++ show (length chkrs) ++")"
  checker <- getLine
  if checker == ""
    then chooseChecker chkrs
    else if read checker > length chkrs || read checker < 1
      then chooseChecker chkrs
      else return $ chosenChecker chkrs (read checker)

{- chosenChecker
   help function to chooseChecker
-}
chosenChecker :: Board -> Int -> Triangle
chosenChecker (x:xs) 1 = x
chosenChecker (x:xs) n = chosenChecker xs (n-1)

{-
amountOfCheckers board checkers
Counts the total amount of checkers on the board
RETURNS: An Int representing the amount of checkers on the board
EXAMPLES:
-}
amountOfCheckers :: Board -> Checkers -> Int
amountOfCheckers (x:xs) checker = amountOfCheckers' $ checkerOptions checker (x:xs)
                                    where
                                      amountOfCheckers' [] = 0
                                      amountOfCheckers' (x:xs) = amountOfCheckers' xs + amountTri x

{-
winCheck board checkers [Int]
Checks if either player has won the game
RETURNS: True if amountOfCheckers board checkers is 0, False otherwise
EXAMPLES:
-}
winCheck :: Board -> Checkers -> Bool
winCheck board checkers = if amountOfCheckers board White == 0
    then  True
    else if amountOfCheckers board Black == 0
        then True
        else False

{-
check if all checkers are in their homeboard
-}
homeBoard :: Checkers -> Board -> Bool
homeBoard checker (x:xs) = if checker == Black
                            then homeBoardBlack $ checkerOptions checker (x:xs)
                            else homeBoardWhite $ reverse $ checkerOptions checker (x:xs)
                            where
                              homeBoardBlack (x:xs) = if position x < 19 then False else True
                              homeBoardWhite (x:xs) = if position x > 6 then False else True


{- offTheBoard checker board
   checks if any checker is outside the board
   RETURNS: a list of any checkers (of color checker) outside the board
-}
offTheBoard :: Checkers -> Board -> Board
offTheBoard checker board = if checker == Black then offTheBoardBlack board else offTheBoardWhite board

offTheBoardWhite :: Board -> Board
offTheBoardWhite [] = []
offTheBoardWhite (x:xs) | x==(Checker White 25 1) = (Checker White 25 1):offTheBoardWhite xs
                        | otherwise = offTheBoardWhite xs

offTheBoardBlack :: Board -> Board
offTheBoardBlack [] = []
offTheBoardBlack (x:xs) | x == (Checker Black 0 1) = (Checker Black 0 1):offTheBoardBlack xs
                        | otherwise = offTheBoardBlack xs

{- validMoves checker dice board
   Looks up which checker-moves are valid
   RETURN: a list of valid checker-moves
-}
validMoves :: Checkers -> [Int] -> Board -> Board
validMoves checker dice board = if homeBoard checker board
                                  then checkerOptions checker board
                                  else (if checker == White
                                  then validMovesWhite (checkerOptions White board) dice board dice
                                  else validMovesBlack (checkerOptions Black board) dice board dice)

-- helper function to validMoves
validMovesBlack :: Board -> [Int] -> Board -> [Int] -> Board
validMovesBlack [] _ _ _ = []
validMovesBlack (x:xs) [] board acc = validMovesBlack xs acc board acc
validMovesBlack (x:xs) a@(die:dice) board acc | (position x+die) > 24 = validMovesBlack (x:xs) dice board acc
                                              | (validMove x (newCheckerPos2 (position x+die) board)) = (x:(validMovesBlack xs acc board acc))
                                              | otherwise = validMovesBlack (x:xs) dice board acc

-- helper function to validMoves
validMovesWhite :: Board -> [Int] -> Board -> [Int] -> Board
validMovesWhite [] _ _ _ = []
validMovesWhite (x:xs) [] board acc = validMovesWhite xs acc board acc
validMovesWhite (x:xs) (die:dice) board acc | (position x-die) < 1 = validMovesWhite (x:xs) dice board acc
                                            | validMove x (newCheckerPos2 (position x-die) board) = x:validMovesWhite (xs) acc board acc
                                            | otherwise = validMovesWhite (x:xs) dice board acc

{- validMovesOffBoard checker board dice
   Checks if any checkers outside the board can reenter it
   RETURNS: a Board with one or zero Triangles, one Triangle if the Triangle outside the board
   can reenter it, and zero if it cannot

-}
validMovesOffBoard :: Checkers -> Board -> [Int] -> Board
validMovesOffBoard checker board dice = if checker == Black
                                          then validMovesOffBoardBlack (offTheBoard checker board) dice board dice
                                          else validMovesOffBoardWhite (offTheBoard checker board) dice board dice

-- helper function to validMovesOffBoard
validMovesOffBoardWhite :: Board -> [Int] -> Board -> [Int] -> Board
validMovesOffBoardWhite (x:xs) (die:[]) board acc | validMove x (newCheckerPos2 (position x-die) board) = x:[]
                                                  | otherwise = []
validMovesOffBoardWhite (x:xs) (die:dice:dicee) board acc | validMove x (newCheckerPos2 (position x-die) board) = x:[]
                                                    | validMove x (newCheckerPos2 (position x-dice) board) = x:[]
                                                    | otherwise = []

-- helper function to validMovesOffBoard
validMovesOffBoardBlack :: Board -> [Int] -> Board -> [Int] -> Board
validMovesOffBoardBlack (x:xs) (die:[]) board acc | validMove x (newCheckerPos2 (position x+die) board) = x:[]
                                                  | otherwise = []
validMovesOffBoardBlack (x:xs) (die:dice:dicee) board acc | validMove x (newCheckerPos2 (position x+die) board) = x:[]
                                                    | validMove x (newCheckerPos2 (position x+dice) board) = x:[]
                                                    | otherwise = []


{- moveChecker checker dice board
   moves the checkers around the board with input from the players
   SIDE EFFECTS: prints Strings and accepts inputs
-}
moveChecker :: Checkers -> [Int] -> Board -> IO ()
moveChecker checker dice a@(x:xs) = do
  if dice == [] || (validMoves checker dice a) == [] || (not (offTheBoard checker a==[]) && (validMovesOffBoard checker a dice == []) )
    then if checker == Black
          then start Black [] a else start White [] a
    else do
  if not (offTheBoard checker a == [])
    then do
      tri <- chooseChecker (offTheBoard checker a)
      die <- chooseDice dice
      newPos <- if checker == Black
                  then newCheckerPos (position tri+die) a
                  else newCheckerPos (position tri-die) a
      if validMove tri newPos
        then if checker == Black
          then start Black (deleteDie die dice) (insertBlack tri a (position newPos))
          else start White (deleteDie die dice) (insertWhite tri a newPos)
        else do
          putStrLn $ "INVALID MOVE"
          moveChecker checker dice a
    else do
  tri <- chooseChecker (validMoves checker dice a)
  die <- chooseDice dice
  if checker == Black
    then (if homeBoard Black a && (position tri+die) > 24
          then start checker (deleteDie die dice) (bearOff tri a)
          else (if (position tri+die > 24 && not (homeBoard Black a))
                  then moveChecker checker dice a
                  else (do
                    newPos <- newCheckerPos (position tri+die) a
                    if validMove tri newPos
                    then start Black (deleteDie die dice) (insertBlack tri a (position tri+die))
                    else do
                      putStrLn $ "INVALID MOVE"
                      moveChecker checker dice a) ) )
    else (if homeBoard White a && (position tri-die) < 1
          then start checker (deleteDie die dice) (bearOff tri a)
          else (if (position tri-die < 1)
                  then do
                    putStrLn $ "INVALID MOVE"
                    moveChecker checker dice a
                  else (do
                    newPos <- newCheckerPos (position tri-die) a
                    if validMove tri newPos
                    then start White (deleteDie die dice) (insertWhite tri a newPos)
                    else do
                      putStrLn $ "INVALID MOVE"
                      moveChecker checker dice a)))


{- bearOff

-}
bearOff :: Triangle -> Board -> Board
bearOff tri [] = []
bearOff tri@(Checker checker pos amount) (x:xs) | tri == x && amount < 2 = (Empty pos 0):bearOff tri xs
                                                | tri == x = (Checker checker pos (amount-1)):bearOff tri xs
                                                | otherwise = x:bearOff tri xs

deleteDie :: Int -> [Int] -> [Int]
deleteDie die (x:xs) = if die == x then xs else x:deleteDie die xs
    --acc == pos
checkerAmountPlus :: Triangle -> Triangle
checkerAmountPlus (Checker a b c) = (Checker a b (c+1))

insertWhite :: Triangle -> Board -> Triangle -> Board
insertWhite tri [] acc = []
insertWhite tri@(Checker checker pos amount) (x:xs) acc | acc == x && isCheckerWhite x = ((checkerAmountPlus x):insertWhite tri xs (acc))
                                                        | acc == x && isCheckerBlack x = ((Checker White (position x) 1):insertWhite tri xs acc) ++ [(Checker Black 0 1)]
                                                        | acc == x = ((Checker White (position x) 1):insertWhite tri xs (acc))
                                                        | tri == x && amount < 2 = (Empty (position tri) 0):xs
                                                        | tri == x = (Checker checker pos (amount-1)):xs
                                                        | otherwise = x:insertWhite tri xs acc

insertBlack :: Triangle -> Board -> Int -> Board
insertBlack tri [] acc = []
insertBlack tri ((Empty pos _):xs) 1 = (Checker Black pos 1):insertBlack tri xs (0)
insertBlack tri ((Checker checker2 pos amount):xs) 1 | Black == checker2 = (Checker Black pos (amount+1)):(insertBlack tri xs 0)
                                                     | White == checker2 = (Checker Black pos 1):(insertBlack tri xs 0) ++ [(Checker White 25 1)]
                                                     | otherwise = (Checker Black pos 1):xs
insertBlack tri@(Checker checker2 pos amount) (x:xs) acc | tri == x && amount < 2 = (Empty pos 0):(insertBlack tri xs (acc-1))
                                                         | tri == x = (Checker checker2 pos (amount-1)):(insertBlack tri xs (acc-1))
                                                         | otherwise = x : insertBlack tri xs (acc-1)


newCheckerPos :: Int -> Board -> IO Triangle
newCheckerPos pos [] = do
  return (Empty 0 0)
newCheckerPos pos (x:xs) = do
  if not (pos == position x)
    then newCheckerPos pos xs
    else return (x)

newCheckerPos2 :: Int -> Board -> Triangle
newCheckerPos2 pos [] = (Empty 0 1)
newCheckerPos2 pos (x:xs) = if pos == position x
                              then x
                              else newCheckerPos2 pos xs

validMove :: Triangle -> Triangle -> Bool
validMove _ (Empty _ 1) = False
validMove _ (Empty _ 0) = True
validMove (Checker White _ _) (Checker check pos amount) | White == check = True
                                                         | Black == check && amount < 2 = True
                                                         | otherwise = False
validMove (Checker Black _ _) (Checker check pos amount) | Black == check = True
                                                         | White == check && amount < 2 = True
                                                         | otherwise = False



test1 = TestCase $ assertEqual "ValidMove (Checker White 1 2) (Empty 3 0)" True (validMove (Checker White 1 2) (Empty 3 0))

test2 = TestCase $ assertEqual "insertBlack" [Checker Black 1 1,Empty 2 0,Checker Black 3 1,Empty 4 0,Empty 5 0,Checker White 6 5,Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,Checker Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,Checker White 24 2] (insertBlack (Checker Black 1 2) newGameState 3)

test3 = TestCase $ assertEqual "insertWhite" [Checker Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,Checker White 6 5,Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,Checker Black 19 5,Empty 20 0,Empty 21 0,Checker White 22 1,Empty 23 0,Checker White 24 1] (insertWhite (Checker White 24 2) newGameState (Empty 22 0))


-- for running all the tests
runtests = runTestTT $ TestList [test1, test2,test3]
