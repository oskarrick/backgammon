type Position = Int
type Move = Int
type Checkers = Int
data Triangle = Empty Position Checkers | White Position Checkers | Black Position Checkers
  deriving (Eq, Show)
type Board = [Triangle]


main :: IO ()

main = undefined

{-EX:
printGameState [Black 1 2,Empty 2 0,Empty 3 0,Empty 4 0,Empty 5 0,White 6 5,Empty 7 0,White 8 3,Empty 9 0,Empty 10 0,Empty 11 0,White 12 5,Black 13 5,Empty 14 0,Empty 15 0,Empty 16 0,Black 17 3,Empty 18 0,Black 19 5,Empty 20 0,Empty 21 0,Empty 22 0,Empty 23 0,White 24 2]
-}
printGameState :: Board -> IO ()

printGameState ((Empty position checkers):[]) = do 
  putStrLn $ "Empty " ++ show position ++ " " ++ show 0 
printGameState ((Empty position checkers):triangles) = do 
  putStrLn $ "Empty " ++ show position ++ " " ++ show 0 
  printGameState triangles
printGameState ((Black position checkers):[]) = do
  putStrLn $ "Black "++ show position ++ " " ++ show checkers 
printGameState ((White position checkers):[]) = do
  putStrLn $ "White "++ show position ++ " " ++ show checkers 
printGameState ((White position checkers):triangles) = do
  putStrLn $ "White "++ show position ++ " " ++ show checkers 
  printGameState triangles
printGameState ((Black position checkers):triangles) = do
  putStrLn $ "Black "++ show position ++ " " ++ show checkers 
  printGameState triangles    

moveCheckers :: Board -> (a, a) -> IO Board 
moveCheckers [triangle] x = undefined


findTriangle :: Board -> Int -> IO ()
findTriangle (Black _ _ :(triangles)) n = undefined

{-validTriangle [White 12 5,Black 13 1,Empty 14 0,Empty 15 0,Empty 16 0,Black 17 3] (White 17 4) 4-}
validTriangle :: Board -> Triangle -> Move -> Maybe (Int)

validTriangle (triangle:[]) (White position checkers) n =
  if validMove triangle && (position - n) == findPosition triangle then Just (position - n)
  else Nothing
validTriangle (triangle:triangles) (White position checkers) n =
  if validMove triangle && (position - n) == findPosition triangle then Just (position - n)
  else validTriangle triangles (White position checkers) n


findPosition :: Triangle -> Position
findPosition (White position checkers) = position
findPosition (Black position checkers) = position
findPosition (Empty position checkers) = position 

chooseAMove :: (Move, Move) -> IO ()
chooseAMove move = do
    print $ "Choose your first move: " ++ show (fst move) ++ " or " ++ show (snd move) 
    choice <- getLine
    return ()
       
validMove :: Triangle -> Bool
validMove (Empty _ _) = True
validMove (White _ _) = True
validMove (Black _ 1) = True
validMove (Black _ _) = False

playMove :: Triangle -> Move -> Triangle
playMove (White position checkers) n = White position (checkers - 1)

playMove' :: Triangle -> Move -> Triangle
playMove' (White position checkers) n = White (position + n) (checkers + 1)

askContinue :: IO ()
askContinue = do
    putStrLn "Start a new game? (Yes or No)"
    str <- getLine
    if (str == "Yes") then main
    else do
        if (str == "No") then do (return ())
        else askContinue

