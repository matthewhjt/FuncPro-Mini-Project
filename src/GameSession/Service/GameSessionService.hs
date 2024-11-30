{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Service.GameSessionService (
    createNewEasySudokuSession,
    findGameSession,
    playGame
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty ( json, pathParam, ActionM, jsonData )
import GameSession.Repository.GameSessionRepository (createGameSession, findGameSessionById, updateGameSession)
import Lib (ApiResponse(..), notFoundResponse, safeCreateObjectId)
import GameSession.Model.GameSessionModel 
    (GameSession(..)
    , PlaySudokuRequest(..)
    , getPlaySudokuRequestMove
    )
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Data.Maybe
import Game.Service.GameGenerator.Sudoku (generateEasySudoku)

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

    case gameSession of
        Just gS -> json gS
        Nothing -> json $ notFoundResponse "Game session not found." Map.empty

playGame :: ActionM ()
playGame = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    prevTurn <- liftIO $ findGameSessionById gameSessionObjectId

    if isNothing prevTurn
    then json $ notFoundResponse "Game session not found." Map.empty
    else do
        let (GameSession gameSessionId gameMoves isWin) = fromJust prevTurn
        req <- jsonData :: ActionM PlaySudokuRequest
        let newMove = getPlaySudokuRequestMove req
        let updatedGameMoves = gameMoves ++ [newMove]
        let newGameSession = GameSession gameSessionId updatedGameMoves isWin
        saveGameSession <- liftIO $ updateGameSession gameSessionId newGameSession
        let response = ApiResponse
                { code = 200
                , success = True
                , message = "Move saved successfully."
                , dataFields = Map.fromList
                    [
                        ("gameSession", toJSON saveGameSession)
                    ]
                }
        json response