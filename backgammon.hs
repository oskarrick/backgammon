module Backgammon (
  newGameState
 ,printGameState
 ,newCheckerPos
 ,position
 ,checkerOptions
 ,chooseChecker
 ,chosenChecker
 ,isCheckerWhite
 ,isCheckerBlack
) where

--import Regler
--import Main
import System.Random
import System.IO
import Data.List

type Position = Int
type Move = Int
type AmountCheckers = Int
data Checkers = Black | White deriving (Show,Eq)
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers
  deriving (Eq, Show)
type Board = [Triangle]
--data State = Running | GameOver (Maybe Player) deriving (Eq, Show)


--main2 :: IO ()
main2 :: Checkers -> [Int] -> Board -> IO Board
main2 checker dice board = do
  printGameState board
  newDice <- calculateMoves
  putStrLn $ "Your moves" ++ show dice
  if dice == []
    then (if checker == Black
            then main2 White newDice board
            else main2 Black newDice board)
  else moveChecker checker dice board


{- newGameState
   Constructs the board as it is at the beginning of the game
-}
newGameState :: Board
newGameState = [Checker Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,Checker White 6 5,
                Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,
                Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,
                Checker Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,Checker White 24 2]

calculateMoves = do
    --putStrLn "Enter any button to roll the dice!"
    --_ <- getLine
    number <- randomRIO (1,6) :: IO Int
    number2 <- randomRIO (1,6) :: IO Int
    if number == number2 then return [number, number, number2, number2]
        else return [number,number2]
{-EX:
printGameState [Checker Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,Checker White 6 5,
                Empty 7 0,Checker White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,Checker White 12 5,
                Checker Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Checker Black 17 3,Empty 18 0,
                Checker Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,Checker White 24 2]
-}
printGame :: IO ()
printGame = do
  printGameState newGameState

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
moveCheckers :: Checkers -> Board -> [Int] -> IO Board
moveCheckers checker board (x:xs) = do
  move <- chooseChecker (checkerOptions checker board) chooseDice (x:xs)
-}

amountOfCheckers :: Board -> Checkers -> Int
amountOfCheckers (x:xs) checker = amountOfCheckers' $ checkerOptions checker (x:xs)
                                    where
                                      amountOfCheckers' (x:[]) = amountTri x
                                      amountOfCheckers' (x:xs) = amountOfCheckers' xs + amountTri x

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

--canUseDie :: Checkers -> Int -> Triangle -> [Int] -> [Int]
canUseDice :: Checkers -> Board -> Int -> [Int] -> [Int]
canUseDice checker board die dice = if homeBoard checker board
                                    then dice
                                    else if checker == Black
                                      then canUseDiceBlack (checkerOptions checker board) die dice
                                      else canUseDiceWhite (reverse (checkerOptions checker board)) die dice

canUseDiceBlack :: Board -> Int -> [Int] -> [Int]
canUseDiceBlack [] _ dice = dice
canUseDiceBlack (x:xs) die dice = if (position x+die) > 24
                                    then (deleteDie die dice)
                                    else canUseDiceBlack xs die dice

canUseDiceWhite :: Board -> Int -> [Int] -> [Int]
canUseDiceWhite [] _ dice = dice
canUseDiceWhite (x:xs) die dice = if (position x-die) < 1
                                    then (deleteDie die dice)
                                    else dice

possibleMoves :: Checkers -> [Int] -> Triangle -> Board -> Board
possibleMoves checker [] tri board = []
possibleMoves checker dice@(x:xs) tri board = if checker == Black
                                                then (if position tri + x > 24
                                                        then possibleMoves checker xs tri board
                                                        else (if validMove tri (newCheckerPos2 (position tri+x) board)
                                                                then newCheckerPos2 (position tri+x) board:possibleMoves checker xs tri board
                                                                else possibleMoves checker xs tri board))
                                                else (if position tri-x < 1
                                                        then possibleMoves checker xs tri board
                                                        else (if validMove tri (newCheckerPos2 (position tri-x) board)
                                                        then (newCheckerPos2 (position tri-x) board):possibleMoves checker xs tri board
                                                        else possibleMoves checker xs tri board))

moveChecker :: Checkers -> [Int] -> Board -> IO Board
moveChecker checker dice a@(x:xs) = do
  tri <- chooseChecker (checkerOptions checker (x:xs))
  die <- chooseDice dice
  --moves <-
  if checker == Black
    then (if (position tri+die > 24 && not (homeBoard Black a))
      then moveChecker checker dice a
      else (do
         newPos <- newCheckerPos (position tri+die) a
         if validMove tri newPos
           then if checker == Black
             then main2 Black (deleteDie die dice) (insertBlack tri a (position tri+die))
             else main2 White (deleteDie die dice) (insertWhite tri a (position tri-die))
           else moveChecker checker dice a))
    else (if (position tri-die < 1 && not (homeBoard White a ))
      then moveChecker checker dice a
      else (do
        newPos <- newCheckerPos (position tri-die) a
        if validMove tri newPos
          then if checker == Black
            then main2 Black (deleteDie die dice) (insertBlack tri a (position tri+die))
            else main2 White (deleteDie die dice) (insertWhite tri a (position tri-die))
          else moveChecker checker dice a))
  --if validMove tri newPos
    --then if checker == Black
      --then main2 Black (deleteDie die dice) (insertBlack tri a (position tri+die))
      --else main2 White (deleteDie die dice) (insertWhite tri a (position tri-die))
    --else moveChecker checker dice a


deleteDie :: Int -> [Int] -> [Int]
deleteDie die (x:xs) = if die == x then xs else x:deleteDie die xs
    --acc == pos
insertWhite :: Triangle -> Board -> Int -> Board
insertWhite tri [] acc = []
insertWhite tri@(Checker checker pos amount) (x:xs) acc | acc == (position x) && isCheckerWhite x = ((Checker White (position x) (amount+2)):insertWhite tri xs (acc))
                                                        | acc == (position x) = ((Checker White (position x) 1):insertWhite tri xs (acc))
                                                        | tri == x && amount < 2 && isCheckerBlack = (Empty (position tri) 0):xs
                                                        | tri == x = (Checker checker pos (amount-1)):xs
                                                        | otherwise = x:insertWhite tri xs acc

insertBlack :: Triangle -> Board -> Int -> Board
insertBlack tri [] acc = []
insertBlack tri ((Empty pos _):xs) 1 = (Checker Black pos 1):xs
insertBlack tri ((Checker checker2 pos amount):xs) 1 | Black == checker2 = (Checker Black pos (amount+1)):xs
                                                     | otherwise = (Checker Black pos 1):xs
insertBlack tri@(Checker checker2 pos amount) (x:xs) acc | tri == x && amount < 2 = (Empty pos 0):(insertBlack tri xs (acc-1))
                                                         | tri == x = (Checker checker2 pos (amount-1)):(insertBlack tri xs (acc-1))
                                                         | otherwise = x : insertBlack tri xs (acc-1)

--moveChecker' a (pos + dice) (x:xs)

chooseDice :: [Int] -> IO Int
chooseDice dices = do
  putStrLn $ "Choose a dice" ++ show dices
  dice <- getLine
  if read dice `elem` dices
    then return (read dice)
    else chooseDice dices

newCheckerPos :: Int -> Board -> IO Triangle
newCheckerPos pos [] = do
  return (Empty 0 0)
newCheckerPos pos (x:xs) = do
  if not (pos == position x)
    then newCheckerPos pos xs
    else return (x)

newCheckerPos2 :: Int -> Board -> Triangle
newCheckerPos2 pos (x:xs) = if not (pos == position x)
                              then newCheckerPos2 pos xs
                              else x

--homeCheckerPos :: IO Triangle
--homeCheckerPos

position :: Triangle -> Int
position (Checker _ pos _) = pos
position (Empty pos _) = pos

amountTri :: Triangle -> Int
amountTri (Empty _ amount) = amount
amountTri (Checker _ _ amount) = amount

{- checkerOptions
   Shows which checkers can, potentially, be moved
-}
checkerOptions :: Checkers -> Board -> Board
checkerOptions _ [] = []
checkerOptions a@White (x:xs) = if isCheckerWhite x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs
checkerOptions a@Black (x:xs) = if isCheckerBlack x
                                then x :(checkerOptions a xs)
                                else checkerOptions a xs

chooseChecker :: Board -> IO Triangle
chooseChecker chkrs = do
  putStrLn $ "Choose a checker (1-" ++ show (length chkrs) ++")"
  print chkrs
  checker <- getLine
  if read checker > length chkrs || read checker < 1
    then chooseChecker chkrs
    else return $ chosenChecker chkrs (read checker)


chosenChecker :: Board -> Int -> Triangle
chosenChecker (x:xs) 1 = x
chosenChecker (x:xs) n = chosenChecker xs (n-1)

isCheckerWhite :: Triangle -> Bool
isCheckerWhite (Checker White _ _) = True
isCheckerWhite _ = False

isCheckerBlack :: Triangle -> Bool
isCheckerBlack (Checker Black _ _) = True
isCheckerBlack _ = False

validMove :: Triangle -> Triangle -> Bool
validMove _ (Empty _ _) = True
validMove (Checker White _ _) (Checker check pos amount) | White == check = True
                                                         | Black == check && amount < 2 = True
                                                         | otherwise = False
validMove (Checker Black _ _) (Checker check pos amount) | Black == check = True
                                                         | White == check && amount < 2 = True
                                                         | otherwise = False
{--

findTriangle :: Board -> Int -> IO ()
findTriangle (Checker Black _ _ :(triangles)) n = undefined

{-validTriangle [White 12 5,Black 13 1,Empty 14 0,Empty 15 0,Empty 16 0,Black 17 3] (White 17 4) 4
validTriangle :: Board -> Triangle -> Move -> Maybe (Int)
-}

--validTriangle (triangle:[]) (Checker White position checkers) n =
  --if Main.validMove triangle && (position - n) == findPosition triangle then Just (position - n)
  --else Nothing
--validTriangle (triangle:triangles) (Checker White position checkers) n =
  --if Main.validMove triangle && (position - n) == findPosition triangle then Just (position - n)
  --else validTriangle triangles (Checker White position checkers) n


findPosition :: Triangle -> Position
findPosition (Checker White position checkers) = position
findPosition (Checker Black position checkers) = position
findPosition (Empty position checkers) = position

chooseAMove :: (Move, Move) -> IO ()
chooseAMove move = do
    print $ "Choose your first move: " ++ show (fst move) ++ " or " ++ show (snd move)
    choice <- getLine
    return ()


{-
validMove :: Triangle -> Bool
validMove (Empty _ _) = True
validMove (Checker White _ _) = True
validMove (Checker Black _ 1) = True
validMove (Checker Black _ _) = False
-}
playMove :: Triangle -> Move -> Triangle
playMove (Checker White position checkers) n = Checker White position (checkers - 1)

playMove' :: Triangle -> Move -> Triangle
playMove' (Checker White position checkers) n = Checker White (position + n) (checkers + 1)
--}
