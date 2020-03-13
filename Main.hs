{--Nadav Spinco 313589996
Shimon Arshavsky 312119126
--}
{-# OPTIONS -Wall #-} 
module Main where


import System.Random
import Minesweeper
import System.Environment 
import Safe (readMay)
main :: IO ()
main = do
    args <- getArgs
    gen <- getStdGen
    if(not(length args == 3)||validInput (args)==False ||Minesweeper.validateArgs (stringToInt(args !! 0)) (stringToInt(args !! 1))(stringToInt(args !! 2))==False )
        then putStrLn("non valid size input exit game")
    else 
        do
        let argsToint= map stringToInt args
        let rowSize = argsToint !! 0
        let colSize = argsToint !! 1
        let numOfMines = argsToint !! 2  
        let bombsGen = Minesweeper.randomListCells (rowSize*colSize) numOfMines [(x,y) | x <- [0..rowSize-1], y <- [0..colSize-1]] gen
        let bombs = takeBombs bombsGen
        let board = Minesweeper.setBoard bombs rowSize colSize
        gameLoop board

--This Function make the game playable
gameLoop :: Board -> IO ()
gameLoop board 
    |(Minesweeper.isWin board) == True  = putStrLn $ (show board) ++ "\n" ++winMessage
    |(Minesweeper.isLose board) == True = putStrLn $ (show board) ++ "\n"++loseMessage
    |otherwise  = do 
         putStrLn $ show board   
         putStrLn ("please enter you next move Dig - 0 Flag-1 , row and col for example: 0 0 0 ")
         act<-getLine 
         let act' = words act
         if (not(length act'==3)||validInput act' == False||(stringToInt (act' !! 0)==1||stringToInt (act' !! 0)==0)==False||Minesweeper.inBounds ((stringToInt (act' !! 1)),(stringToInt (act' !! 2))) board==False)
         then
              do
                 putStrLn("non valid play please press again" )
                 gameLoop board
         else
            do
             let actChoise = (stringToInt (act' !! 0))
             let rowChoise = (stringToInt (act' !! 1))
             let colChoise = (stringToInt (act' !! 2))
             let newBoard = Minesweeper.actMove actChoise (rowChoise,colChoise) board
             gameLoop newBoard
        
         
--convert string to int
stringToInt:: String -> Int
stringToInt s = (read s :: Int)

--take all the bombs after the random action 
takeBombs:: [(Cell,StdGen)]->[Cell]
takeBombs []=[]
takeBombs ((c,_):lst)= c:(takeBombs lst)

winMessage:: String
winMessage = "you won!"

loseMessage:: String
loseMessage= "boom! , you lost!"
--used for deal with wrong Data type
sToInt:: String-> Int
sToInt s = fromReadMay (readMay s :: Maybe Int) 
--To convert From Maybe
fromReadMay:: (Maybe Int) -> Int
fromReadMay Nothing = -1
fromReadMay (Just x) = x

--to check if input is valid (Data type)
validInput:: [String]->Bool
validInput lst 
 |sToInt (lst!!0) == (-1) = False
 |sToInt (lst!!1) == (-1) = False
 |sToInt (lst!!2) == (-1) = False 
 |otherwise = True