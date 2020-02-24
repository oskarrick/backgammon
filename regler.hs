data Checkers = Black | White deriving (Show,Eq)

type Board = (Int, [Checkers])

canMove :: Checkers -> Board -> Bool
canMove _ (_,[]) = True
canMove White (x,l) | White == head l && x > 12 = True
                    | White == head l && x <= 12 = False
                    | [Black] == l = True
                    | otherwise = False
canMove Black (x,l) | Black == head l && x <= 12 = True
                    | Black == head l && x > 12 = False
                    | [White] == l = True
                    | otherwise = False

canMove2 :: Checkers -> Board -> Int -> Bool
canMove2 White (x,l) dice | (x+dice) > 24 = False
                          | otherwise = True
canMove2 Black (x,l) dice | x < 13 && (x+dice) > 12 = False
                          | otherwise = True
