{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, put, post, scotty, formParam, ScottyM, ActionM )
import Data.Text.Lazy (Text, toStrict)
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService 
    ( createSudokuSession
    , findGameSession
    , playSudoku
    )
import Auth.Service.AuthService (registerUser, login)
import Auth.Security.AuthMiddleware (authMiddleware)

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/:difficulty" $ authMiddleware createSudokuSession
    get "/gameSession/:gameSessionId" $ authMiddleware findGameSession
    put "/playGame/sudoku/:gameSessionId" $ authMiddleware playSudoku
    
    post "/register" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        registerUser (toStrict uname) (toStrict pwd)

    post "/login" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        login (toStrict uname) (toStrict pwd)

main :: IO ()
main = do
    scotty 3001 funpro
