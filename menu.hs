import System.Random
import System.IO

data Menu menu

menu :: IO ()
menu = do
    putStrLn ("MENU")
    putStrLn ("1. Start game")
    putStrLn ("2. Quit")
    choice <- getLine
    if choice == "1" then startGame
    else if choice == "2" then return ()
    else menu


startGame :: IO ()
startGame = do 
    number1 <- randomRIO (1,6) :: IO Int
    putStrLn "Player 1, press any button to roll the dice" 
    _ <- getLine
    putStrLn (show number1)
    number2 <- randomRIO (1,6) :: IO Int
    putStrLn "Player 2, press any button to roll the dice"
    _ <- getLine
    putStrLn (show number2)
    if number1 > number2 then putStrLn ("Player 1 starts!")
    else if number2 > number1 then putStrLn ("Player 2 starts!")
    else startGame