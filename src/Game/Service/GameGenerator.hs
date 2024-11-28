module Game.Service.GameGenerator
    ( generateEasySudoku, generateMediumSudoku, generateHardSudoku
    ) where

import System.Random (randomRIO)
import Data.List (delete)
import Control.Monad (forM_, foldM)
import Data.Maybe (isJust)
import Game.Service.GameValidator (isValidSudokuBoard, Board)

emptyBoard :: Board
emptyBoard = replicate 9 (replicate 9 0)

generateEasySudoku :: IO Board
generateEasySudoku = generateSudoku 30

generateMediumSudoku :: IO Board
generateMediumSudoku = generateSudoku 45

generateHardSudoku :: IO Board
generateHardSudoku = generateSudoku 60

generateSudoku :: Int -> IO Board
generateSudoku numHoles = do
  board <- fillDiagonal emptyBoard
  let fullBoard = fillRemaining board 0 3
  removeNumbers fullBoard numHoles

fillDiagonal :: Board -> IO Board
fillDiagonal board = foldM (\b i -> fillBox b i i) board [0, 3, 6]

fillBox :: Board -> Int -> Int -> IO Board
fillBox board row col = do
  nums <- shuffle [1..9]
  let positions = [(r, c) | r <- [row..row+2], c <- [col..col+2]]
  return $ placeNumbers nums board positions

placeNumbers :: [Int] -> Board -> [(Int, Int)] -> Board
placeNumbers [] board _ = board
placeNumbers _ board [] = board
placeNumbers (n:ns) board ((r, c):rcs) =
  let newBoard = replace2D r c n board
  in placeNumbers ns newBoard rcs

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (front, x:back) = splitAt i xs
  fmap (x :) (shuffle (front ++ back))

fillRemaining :: Board -> Int -> Int -> Board
fillRemaining board i j
  | i >= 9 = board
  | j >= 9 = fillRemaining board (i + 1) 0
  | board !! i !! j /= 0 = fillRemaining board i (j + 1)
  | otherwise = tryNums board i j [1..9]
  where
    tryNums :: Board -> Int -> Int -> [Int] -> Board
    tryNums b _ _ [] = b
    tryNums b x y (n:ns)
      | isSafe b x y n =
          let newBoard = replace2D x y n b
          in if isBoardFilled newBoard
             then newBoard
             else fillRemaining newBoard x (y + 1)
      | otherwise = tryNums b x y ns


isBoardFilled :: Board -> Bool
isBoardFilled = all (all (/= 0))

isSafe :: Board -> Int -> Int -> Int -> Bool
isSafe board row col num = isJust $ isValidSudokuBoard $ Just $ replace2D row col num board

removeNumbers :: Board -> Int -> IO Board
removeNumbers board 0 = return board
removeNumbers board n = do
  (row, col) <- randomEmptyCell board
  let newBoard = replace2D row col 0 board
  removeNumbers newBoard (n - 1)

randomEmptyCell :: Board -> IO (Int, Int)
randomEmptyCell board = do
  row <- randomRIO (0, 8)
  col <- randomRIO (0, 8)
  if board !! row !! col /= 0
    then return (row, col)
    else randomEmptyCell board

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D row col newVal board =
  take row board ++
  [take col (board !! row) ++ [newVal] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board