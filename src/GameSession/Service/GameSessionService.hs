{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Service.GameSessionService (
    createSudokuSession,
    findGameSession,
    playGame
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty ( json, pathParam, ActionM, jsonData, status )
import GameSession.Repository.GameSessionRepository (createGameSession, findGameSessionById, updateGameSession)
import Lib (ApiResponse(..), successResponse, errorResponse, notFoundResponse, safeCreateObjectId)
import GameSession.Model.GameSessionModel 
    (GameSession(..)
    , PlaySudokuRequest(..)
    )
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Data.Maybe
import Game.Service.GameGenerator.Sudoku (generateEasySudoku, generateMediumSudoku, generateHardSudoku, isBoardFilled)
import Game.Service.GameValidator.GameValidator (isValidSudokuBoard)
import Network.HTTP.Types.Status (badRequest400, notFound404)

createSudokuSession :: ActionM ()
createSudokuSession = do
    difficulty <- pathParam "difficulty" :: ActionM String
    sudoku <- case difficulty of 
        "easy"   -> liftIO generateEasySudoku
        "medium" -> liftIO generateMediumSudoku
        "hard"   -> liftIO generateHardSudoku
        _        -> liftIO generateEasySudoku
    liftIO $ print sudoku
    gameSessionId <- liftIO $ createGameSession sudoku
    let gameSession = GameSession gameSessionId [sudoku] False
    let response = successResponse 200 "Game created successfully" $ Map.fromList [("gameSession", toJSON gameSession)]
    json response

findGameSession :: ActionM()
findGameSession = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    gameSession <- liftIO $ findGameSessionById gameSessionObjectId

    case gameSession of
        Just gS -> json gS
        Nothing -> do
            status notFound404
            json $ notFoundResponse "Game session not found." Map.empty

playGame :: ActionM ()
playGame = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    prevTurn <- liftIO $ findGameSessionById gameSessionObjectId

    if isNothing prevTurn
    then json $ notFoundResponse "Game session not found." Map.empty
    else do
        let (GameSession gameSessionId gameMoves _) = fromJust prevTurn
        req <- jsonData :: ActionM PlaySudokuRequest
        let newMove = move req
        let validMove = isValidSudokuBoard $ Just newMove
        case validMove of
            Nothing -> do
                status badRequest400
                json $ errorResponse 400 "Invalid move." Map.empty
            Just vm -> do
                let sudokuWin = isBoardFilled vm
                let updatedGameMoves = gameMoves ++ [newMove]
                let newGameSession = GameSession gameSessionId updatedGameMoves sudokuWin
                saveGameSession <- liftIO $ updateGameSession gameSessionId newGameSession
                let responseMsg = 
                        if sudokuWin
                        then "Game won."
                        else "Move saved successfully."

                let response = ApiResponse
                        { code = 200
                        , success = True
                        , message = responseMsg
                        , dataFields = Map.fromList
                            [
                                ("gameSession", toJSON saveGameSession)
                            ]
                        }
                json response