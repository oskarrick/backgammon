type Position = Int
type Move = Int
type AmountCheckers = Int
data Triangle = Empty Position AmountCheckers | Checker Checkers Position AmountCheckers
  deriving (Eq, Show)
type Board = [Triangle]

data Checkers = Black | White deriving (Show,Eq)

--type Board = (Int, [Checkers])

validMove :: Triangle -> Triangle -> Bool
validMove _ (Empty _ _) = True
validMove (Checker White _ _) (Checker check pos amount) | White == check = True
                                                         | Black == check && amount < 2 = True
                                                         | otherwise = False
validMove (Checker Black _ _) (Checker check pos amount) | Black == check = True
                                                         | White == check && amount < 2 = True
                                                         | otherwise = False




{-
canMove2 :: Checkers -> Board -> Int -> Bool
canMove2 White (x,l) dice | (x+dice) > 24 = False
                          | otherwise = True
canMove2 Black (x,l) dice | x < 13 && (x+dice) > 12 = False
                          | otherwise = True
-}
