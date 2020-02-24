type Position = Int
type Move = Int
type AmountCheckers = Int
data Triangle = Empty Position Checkers | Checkers Position AmountCheckers | Checkers Position AmountCheckers
  deriving (Eq, Show)
type Board = [Triangle]

Checkers = Black | White deriving (Show,Eq)

--type Board = (Int, [Checkers])

validMove :: Checkers -> Board -> Bool
validMove _ (Empty _ _) = True
validMove White (check pos amount) | White == check && pos > 12 = True
                                   | White == check && pos <= 12 = False
                                   | Black == check && amount < 2 = True
                                   | otherwise = False
validMove Black (check pos amount) | Black == check && pos <= 12 = True
                                   | Black == check && pos > 12 = False
                                   | White == check && amount < 2 = True
                                   | otherwise = False


{-
canMove2 :: Checkers -> Board -> Int -> Bool
canMove2 White (x,l) dice | (x+dice) > 24 = False
                          | otherwise = True
canMove2 Black (x,l) dice | x < 13 && (x+dice) > 12 = False
                          | otherwise = True
-}
