module GameSession.Service.Generate
    ( generateSudoku
    ) where

import System.Random (randomRIO)
import Data.List (delete)
import Control.Monad (forM_)
import Game.Service.GameValidator
import Data.Maybe (isJust)

type Board = [[Int]]

emptyBoard :: Board
emptyBoard = replicate 9 (replicate 9 0)

generateSudoku :: Int -> IO Board
generateSudoku numHoles = do
  board <- fillDiagonal emptyBoard
  fullBoard <- fillRemaining board 0 3
  removeNumbers fullBoard numHoles

fillDiagonal :: Board -> IO Board
fillDiagonal board = foldl (\acc i -> acc >>= \b -> fillBox b i i) (return board) [0, 3, 6]

fillBox :: Board -> Int -> Int -> IO Board
fillBox board row col = do
  nums <- shuffle [1..9]
  return $ foldl (\b (i, n) -> replace2D (row + i `div` 3) (col + i `mod` 3) n b) board (zip [0..8] nums)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (front, x:back) = splitAt i xs
  fmap (x :) (shuffle (front ++ back))

fillRemaining :: Board -> Int -> Int -> IO Board
fillRemaining board i j
  | i >= 9 = return board
  | j >= 9 = fillRemaining board (i + 1) 0
  | board !! i !! j /= 0 = fillRemaining board i (j + 1)
  | otherwise = do
      nums <- shuffle [1..9]
      tryNums board i j nums
  where
    tryNums b _ _ [] = return b
    tryNums b x y (n:ns)
      | isSafe b x y n = do
          let newBoard = replace2D x y n b
          result <- fillRemaining newBoard x (y + 1)
          if isBoardFilled result then return result else tryNums b x y ns
      | otherwise = tryNums b x y ns

isBoardFilled :: Board -> Bool
isBoardFilled = all (all (/= 0))

isSafe :: Board -> Int -> Int -> Int -> Bool
isSafe board row col num = isJust $ isValidSudokuBoard $ Just $ replace2D row col num board

removeNumbers :: Board -> Int -> IO Board
removeNumbers board 0 = return board
removeNumbers board n = do
  row <- randomRIO (0, 8)
  col <- randomRIO (0, 8)
  if board !! row !! col /= 0
    then do
      let newBoard = replace2D row col 0 board
      removeNumbers newBoard (n - 1)
    else removeNumbers board n

replace2D :: Int -> Int -> a -> [[a]] -> [[a]]
replace2D row col newVal board =
  take row board ++
  [take col (board !! row) ++ [newVal] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board