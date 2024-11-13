{-# OPTIONS_GHC -Wno-missing-fields #-}
module GameSession.Service.GameSessionService (
    createNewEasySudokuSession
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty (json, ActionM)
import Game.Service.GameGenerator (generateEasySudoku)
import GameSession.Repository.GameSessionRepository
import Game.Service.GameValidator (Board)
import Lib (ApiResponse(..))
import GameSession.Model.GameSessionModel (GameSession(..))

createNewEasySudokuSession :: ActionM()
createNewEasySudokuSession = do
    let easySudoku = [[5,0,1,6,0,9,8,2,7],[6,0,2,8,0,0,0,0,9],[9,0,4,0,0,0,0,0,3],[4,0,6,2,1,8,7,5,0],[1,0,7,3,0,0,0,0,4],[0,0,2,0,0,5,0,9,0],[0,7,4,0,8,0,0,0,0],[0,2,0,0,6,3,0,0,5],[0,0,1,0,0,0,3,7,0]] :: Board
    gameSessionId <- liftIO $ createGameSession easySudoku
    liftIO $ putStrLn $ "Created Game Session ID: " ++ show gameSessionId
    let gameSession = GameSession gameSessionId [easySudoku] False
    let response = ApiResponse
            { code = 200
            , success = True
            , message = "Game created successfully."
            , dataField = gameSession
            }
    json response
