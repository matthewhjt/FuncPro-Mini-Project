{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Service.GameSessionService (
    createNewEasySudokuSession,
    findGameSession
    -- playGame
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty ( json, pathParam, ActionM )
import GameSession.Repository.GameSessionRepository (createGameSession, findGameSessionById)
import Game.Service.GameValidator (Board)
import Lib (ApiResponse(..), safeCreateObjectId)
import GameSession.Model.GameSessionModel (GameSession(..))
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Game.Service.GameGenerator (generateEasySudoku)

createNewEasySudokuSession :: ActionM()
createNewEasySudokuSession = do
    easySudoku <- liftIO generateEasySudoku
    gameSessionId <- liftIO $ createGameSession easySudoku
    let gameSession = GameSession gameSessionId [easySudoku] False
    let response = ApiResponse
            { code = 200
            , success = True
            , message = "Game created successfully."
            , dataFields = Map.fromList
                [
                    ("gameSession", toJSON gameSession)
                ]
            }
    json response

findGameSession :: ActionM()
findGameSession = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    gameSession <- liftIO $ findGameSessionById gameSessionObjectId

    let notFoundResponse = ApiResponse
                        { code = 404
                        , success = False
                        , message = "GameSession Not Found"
                        , dataFields = Map.empty
                        }

    case gameSession of
        Just gS -> json gS
        Nothing -> json notFoundResponse
