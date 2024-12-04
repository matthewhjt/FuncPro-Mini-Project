{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty (get, put, post, scotty, json, jsonData, formParam, param, ScottyM, ActionM, rescue, middleware)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, toStrict)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.MongoDB as Mongo
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService (createNewEasySudokuSession, findGameSession, playGame)
import Auth.Service.AuthService (registerUser, login)
import Auth.Security.AuthMiddleware (authMiddleware)
import Haskemon.Service.HaskemonService (createHaskemonForUser, createGameSession, findHaskemonSession, playHaskemon)
import Web.Scotty.Internal.Types (ScottyException)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib (safeCreateObjectId, corsMiddleware)

handleParam :: ActionM a -> a -> ActionM a
handleParam action defaultValue = action `rescue` (\(_ :: ScottyException) -> return defaultValue)

createHaskemonEndpoint :: ScottyM ()
createHaskemonEndpoint = post "/haskemon" $ authMiddleware $ \username -> do
    name <- handleParam (formParam "name") "DefaultHaskemon"
    hp <- handleParam (formParam "healthPoint") 0
    mana <- handleParam (formParam "mana") 0
    atk <- handleParam (formParam "attack") 0
    def <- handleParam (formParam "defense") 0
    element <- handleParam (formParam "element") "Fire"

    response <- liftIO $ createHaskemonForUser username name hp mana atk def element
    json response


createHaskemonSessionEndpoint :: ScottyM ()
createHaskemonSessionEndpoint = post "/gameSession" $ authMiddleware $ \username -> do
    teamIdsStr <- handleParam (formParam "team") ""
    let teamIds = mapMaybe safeCreateObjectId (splitOn "," teamIdsStr)
    
    liftIO $ print teamIds
    response <- liftIO $ createGameSession username teamIds
    json response

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/easy" $ authMiddleware $ \_ -> createNewEasySudokuSession
    get "/gameSession/:gameSessionId" findGameSession
    put "/gameSession/:gameSessionId" playGame
    get "/haskemonSession/:haskemonSessionId" findHaskemonSession
    put "/haskemonSession/:haskemonSessionId" playHaskemon
    
    post "/register" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        registerUser (toStrict uname) (toStrict pwd)

    post "/login" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        login (toStrict uname) (toStrict pwd)

    createHaskemonEndpoint
    createHaskemonSessionEndpoint

main :: IO ()
main = do
    scotty 3001 (do
        middleware corsMiddleware
        funpro)
