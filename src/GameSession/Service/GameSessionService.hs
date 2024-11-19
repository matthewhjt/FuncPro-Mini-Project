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

createNewEasySudokuSession :: ActionM()
createNewEasySudokuSession = do
    let easySudoku = [[5,0,1,6,0,9,8,2,7],[6,0,2,8,0,0,0,0,9],[9,0,4,0,0,0,0,0,3],[4,0,6,2,1,8,7,5,0],[1,0,7,3,0,0,0,0,4],[0,0,2,0,0,5,0,9,0],[0,7,4,0,8,0,0,0,0],[0,2,0,0,6,3,0,0,5],[0,0,1,0,0,0,3,7,0]] :: Board
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
