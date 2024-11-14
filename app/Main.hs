{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, post, scotty, jsonData, formParam, ScottyM, ActionM )
import Data.Text.Lazy (Text, toStrict)
import Game.Service.GameService (getAllGames)
import Auth.Service.AuthService (registerUser, login)
import Auth.Security.AuthMiddleware (authMiddleware)
import Auth.Model.UserModel (User(..))

funpro :: ScottyM()
funpro = do
    get "/games" $ authMiddleware getAllGames

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
