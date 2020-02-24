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
    putStrLn (show number ++ " and " ++ show number2)