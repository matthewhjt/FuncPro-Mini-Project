{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, scotty, ScottyM, put )
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService (createNewEasySudokuSession, findGameSession)

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/easy" createNewEasySudokuSession 
    get "/gameSession/:gameSessionId" findGameSession
    -- put "/gameSession/:gameSessionId" playGame

main :: IO ()
main = do
    scotty 3000 funpro