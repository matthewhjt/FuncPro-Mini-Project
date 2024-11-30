{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Web.Scotty (get, put, post, scotty, json, jsonData, formParam, param, ScottyM, ActionM, rescue)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, toStrict)
import qualified Data.Map as Map
import Game.Service.GameService (getAllGames)
import GameSession.Service.GameSessionService (createNewEasySudokuSession, findGameSession, playGame)
import Auth.Service.AuthService (registerUser, login)
import Auth.Security.AuthMiddleware (authMiddleware)
import Haskemon.Service.HaskemonService (createHaskemonForUser, createGameSession)
import Web.Scotty.Internal.Types (ScottyException)

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


createGameSessionEndpoint :: ScottyM ()
createGameSessionEndpoint = post "/gameSession" $ authMiddleware $ \username -> do
    team1Ids <- formParam "team1" `rescue` const (return [])
    team2Ids <- formParam "team2" `rescue` const (return [])
    result <- liftIO $ createGameSession username team1Ids team2Ids
    case result of
        Left err -> json ApiResponse
            { code = 400
            , success = False
            , message = err
            , dataFields = Map.empty
            }
        Right session -> json ApiResponse
            { code = 200
            , success = True
            , message = "Game session created successfully."
            , dataFields = Map.fromList [("sessionId", toJSON $ show (sessionId session))]
            }

funpro :: ScottyM()
funpro = do
    get "/games" getAllGames
    get "/gameSession/newGame/sudoku/easy" $ authMiddleware $ \_ -> createNewEasySudokuSession
    get "/gameSession/:gameSessionId" findGameSession
    put "/gameSession/:gameSessionId" playGame
    
    post "/register" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        registerUser (toStrict uname) (toStrict pwd)

    post "/login" $ do
        uname <- formParam "username" :: ActionM Text
        pwd <- formParam "password" :: ActionM Text
        login (toStrict uname) (toStrict pwd)

    createHaskemonEndpoint

main :: IO ()
main = do
    scotty 3000 funpro
