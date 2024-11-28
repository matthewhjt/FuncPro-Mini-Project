{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, put, post, scotty, jsonData, formParam, ScottyM, ActionM )
import Data.Text.Lazy (Text, toStrict)
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService (createNewEasySudokuSession, findGameSession)
import Auth.Service.AuthService (registerUser, login)
import Auth.Security.AuthMiddleware (authMiddleware)
import Auth.Model.UserModel (User(..))

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/easy" $ authMiddleware createNewEasySudokuSession
    get "/gameSession/:gameSessionId" findGameSession
    -- put "/gameSession/:gameSessionId" playGame
    
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
    scotty 3000 funpro
