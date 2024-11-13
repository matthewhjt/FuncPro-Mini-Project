{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, scotty, ScottyM )
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService (createNewEasySudokuSession)

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/easy" createNewEasySudokuSession 

main :: IO ()
main = do
    scotty 3000 funpro