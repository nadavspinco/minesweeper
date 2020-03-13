
{-# OPTIONS -Wall #-} 
module Minesweeper
    ( 
      Board
      ,validateArgs
      ,Cell
      ,GameStatus
      ,setBoard
      ,checkForWin
      ,showBoard
      ,inBounds
      ,digAct
      ,toggleFlag
      ,randomListCells
      ,actMove
      ,isWin
      ,isLose
    ) where
import Data.List
import System.Random
data GameStatus = ON|WIN|LOSE deriving (Show,Read,Eq)

type Cell = (Int, Int)   
data Board = Board { rowSize :: Int
                     , colSize :: Int
                     , bombs :: [Cell]
                     , maskedCells ::  [Cell]
                     , clickedCells :: [Cell] 
                     ,flaggedCells :: [Cell]
                     , gameSt::GameStatus
                     } 


--check the input for the board is Valid
validateArgs:: Int -> Int-> Int ->Bool
validateArgs row col mines = 
     (row>=10)&& (row<=20)
     &&(col>=10)&& (col<=20)&& 
     (mines>=4)&&(mines<=200)
     && (mines<=row*col -1)

--initialize the board with all of the data for a new game
--takes list of bombs and 2 ints for the row and col size
setBoard::[Cell]-> Int->Int->Board 
setBoard b row cols = Board {rowSize= row,colSize=cols,bombs=b
,maskedCells=[(x,y)| x <- [0..row-1], y <- [0..cols-1]],
clickedCells=[],flaggedCells=[],gameSt=ON }


flaggedCell :: String
flaggedCell = "[ ! ]"

maskedCell::String
maskedCell = "[   ]"

bombCell :: String
bombCell = "[ * ]"

--This function check if there is a win by checking if the player clicked all of the unbomb cells
checkForWin:: Board ->Board
checkForWin (Board row cols bombsLst masked clicked flaged gameStat)
  |gameStat == LOSE = (Board row cols bombsLst masked clicked flaged gameStat)
  |(length clicked) == ((row * cols) - (length bombsLst)) = (Board row cols bombsLst masked clicked flaged WIN) 
  |otherwise = (Board row cols bombsLst masked clicked flaged gameStat)

--This function return true if there is a win else return false
isWin :: Board -> Bool
isWin (Board _ _ _ _ _ _ gameStat) 
  |gameStat== WIN =True
  |otherwise= False

--This function return true if there is a Lose else return false
isLose :: Board -> Bool
isLose (Board _ _ _ _ _ _ gameStat) 
  |gameStat== LOSE =True
  |otherwise= False


--instance Show for printing the board in the wanted pattern 
instance Show Board where
  show b = 
     unlines $  (showBoard b ((rowSize b)) (colSize b))
--used for getting a sign for every cell
cellSign:: Board-> Cell->String
cellSign (Board row cols bombsLst masked clicked flaged gameStat) c
    |((searchList masked c) == True) = maskedCell
    |((searchList flaged c) == True) = flaggedCell
    |((searchList bombsLst c) == True) =  bombCell
    |((searchList clicked c) == True) =clickedCellSign (Board row cols bombsLst masked clicked flaged gameStat) c
    |otherwise = maskedCell
--evaluate the sign of clicked Cell (num)
clickedCellSign:: Board->Cell->String
clickedCellSign (Board row cols bombsLst masked clicked flaged gameStat) c=
     ("[ "++show (bombNeighbors (neighbors c (Board row cols bombsLst masked clicked flaged gameStat) ) bombsLst)++" ]")
--The function function return true if the a cell is in a list of cells else false
searchList:: [Cell]->Cell->Bool
searchList [] _ = False 
searchList (c:lst) cell 
    |c == cell = True 
    |otherwise = searchList lst cell

--show the board, for the instance show
showBoard:: Board->Int->Int->[String]
showBoard _ 0 colC =(printColNumbers colC 0):[]
showBoard b rowC colC = (showBoard b (rowC-1) colC)++[showRow b (rowC-1) colC]

--show the row ,for the showBoard
showRow:: Board->Int->Int ->String
showRow _ rowNum 0 = (addZero rowNum) ++ show rowNum ++ " "
showRow b rowNum colNum =  (showRow b rowNum (colNum-1))++ " " ++ (cellSign b (rowNum,colNum-1))
--print the colNumbers, for the instance show
printColNumbers :: Int -> Int -> String
printColNumbers 0 _ = ""
printColNumbers ctr 0 = "     " ++ "00" ++ "    " ++ printColNumbers (ctr - 1) (1)
printColNumbers ctr nr =(addZero nr) ++ show  nr ++ "    " ++ printColNumbers (ctr - 1) (nr + 1)
-- add before the number for the board format
addZero:: Int -> String
addZero nr
 |nr<10 = "0"
 |otherwise = ""
--the function checks if a cell is in the board right range
inBounds :: Cell -> Board -> Bool
inBounds (x,y) board =
    let w = rowSize board
        h = colSize board
    in
        ((-1) < x) && 
        (x < w) && 
        ((-1) < y) && 
        (y < h)
--The function gets a cell and a board and return all of the neighbors of the call in the board, assuming the cell is valid

neighbors :: Cell -> Board -> [Cell]
neighbors (x,y) b = [(adjX, adjY) | adjX <- [x-1..x+1], 
                                    adjY <- [y-1..y+1],
                                    not $ (adjX == x) && (adjY == y), -- a cell isn't its own neighbor
                                    inBounds (adjX, adjY) b] 
--This function get a list of cells and a secoend list of bombs return the number of bombs in the first one
bombNeighbors :: [Cell]-> [Cell]->Int
bombNeighbors [] _ = 0
bombNeighbors (x:xs) allBombs 
  |(searchList allBombs x)==True = 1 + bombNeighbors xs allBombs
  |otherwise = bombNeighbors xs allBombs
-- call to the right move action and then check for win
actMove:: Int->Cell->Board->Board
actMove 0 cell b =checkForWin (digAct  b cell)
actMove 1 cell b =checkForWin (toggleFlag b cell)
actMove _ _ b = b

--perform the dig act
digAct :: Board->Cell->Board
digAct  (Board row cols bombsLst masked clicked flaged gameStat) c
    |searchList (flaged )c== True = (Board row cols bombsLst masked clicked flaged gameStat)
    |searchList (bombsLst) c == True = (Board row cols bombsLst [] clicked flaged LOSE)
    |searchList (clicked) c == True = (Board row cols bombsLst masked clicked flaged gameStat)
    |clickedCellSign (Board row cols bombsLst masked clicked flaged gameStat) c == "[ 0 ]"= 
      digEmptyCells (Board row cols bombsLst (delete c masked) (c:clicked) flaged gameStat) 
      (emptyneighbors c (Board row cols bombsLst (delete c masked) (c:clicked) flaged gameStat))
    |otherwise = (Board row cols bombsLst (delete c masked) (c:clicked) flaged gameStat)
--perform the toggle Flag act
toggleFlag:: Board-> Cell-> Board
toggleFlag (Board row cols bombsLst masked clicked flaged gameStat) c
  |searchList (flaged) c ==True =(Board row cols bombsLst (c:masked) clicked (delete c flaged) gameStat)
  |searchList (clicked) c ==True =(Board row cols bombsLst masked clicked flaged gameStat) 
  |otherwise =(Board row cols bombsLst (delete c masked) clicked (c:flaged) gameStat)
--dig empty cells and neibours like in the game
digEmptyCells:: Board -> [Cell] ->Board
digEmptyCells (Board row cols bombsLst masked clicked flaged gameStat) [] = (Board row cols bombsLst masked clicked flaged gameStat)
digEmptyCells (Board row cols bombsLst masked clicked flaged gameStat) (c:lst) = digEmptyCells (digAct  (Board row cols bombsLst masked clicked flaged gameStat) c) lst
--take all of the neighbors of a cell to make the digEmptyCells func
emptyneighbors:: Cell -> Board -> [Cell]
emptyneighbors (x,y) b = [(adjX, adjY) | adjX <- [x-1..x+1], 
                                    adjY <- [y-1..y+1],
                                    not $ (adjX == x) && (adjY == y), -- a cell isn't its own neighbor
                                    inBounds (adjX, adjY) b,searchList (clickedCells b) ((adjX, adjY)) ==False] 

--this function random a number in the range and return diffrent StdGen
--take from :Learn You a Haskell for Great Good!
randomCell :: Int -> StdGen -> (Int, StdGen)
randomCell size g  =
  let (rand, g') = randomR (0, size- 1) g 
  in (rand, g')
-- this function perform the random of the bombs list, get the boardSize the wanted num of mines and the optional cell
randomListCells :: Int -> Int -> [Cell] -> StdGen ->[(Cell,StdGen)]
randomListCells _ 0 _ _ = []
randomListCells _ _ [] _ = []
randomListCells sizeBoard numOfMines allCells gen =
    let (cel,gen')= randomCell (sizeBoard)  gen
    in [((allCells !! cel),gen')] ++ randomListCells (sizeBoard -1) (numOfMines-1) (delete (allCells !! cel) allCells) gen'

