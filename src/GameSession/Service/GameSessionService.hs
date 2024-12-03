{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
module GameSession.Service.GameSessionService (
    createSudokuSession,
    findGameSession,
    playSudoku
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Web.Scotty ( json, pathParam, ActionM, jsonData, status )
import GameSession.Repository.GameSessionRepository (createGameSession, findGameSessionById, updateGameSession)
import Lib (successResponse, errorResponse, notFoundResponse, safeCreateObjectId)
import GameSession.Model.GameSessionModel
    (GameSession(..)
    , PlaySudokuRequest(..), SomeGameSession (..)
    )
import qualified Data.Map as Map
import Data.Aeson (toJSON)
import Game.Service.GameGenerator.Sudoku 
    (generateEasySudoku
    , generateMediumSudoku
    , generateHardSudoku
    , isBoardFilled
    , safeReplace2D
    )
import Game.Service.GameValidator.GameValidator (isValidSudokuBoard)
import Network.HTTP.Types.Status (badRequest400, notFound404)
import Database.MongoDB (genObjectId, val)

createSudokuSession :: ActionM ()
createSudokuSession = do
    difficulty <- pathParam "difficulty" :: ActionM String
    sudoku <- case difficulty of
        "easy"   -> liftIO generateEasySudoku
        "medium" -> liftIO generateMediumSudoku
        "hard"   -> liftIO generateHardSudoku
        _        -> liftIO generateEasySudoku
    gameSessionId <- liftIO genObjectId
    let gameSession = SudokuGameSession (val gameSessionId) [sudoku] False
    _ <- liftIO $ createGameSession gameSession
    let response = successResponse 200 "Game created successfully" $ Map.fromList [("gameSession", toJSON gameSession)]
    json response

findGameSession :: ActionM()
findGameSession = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    gameSession <- liftIO $ findGameSessionById gameSessionObjectId

    case gameSession of
        Just gS -> json $ successResponse 200 "Game session found" $ Map.fromList [("gameSession", toJSON gS)]
        Nothing -> do
            status notFound404
            json $ notFoundResponse "Game session not found." Map.empty

playSudoku :: ActionM ()
playSudoku = do
    gameSessionId <- pathParam "gameSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId gameSessionId
    prevTurn <- liftIO $ findGameSessionById gameSessionObjectId

    case prevTurn of
        Nothing -> do
            status notFound404
            json $ notFoundResponse "Game session not found." Map.empty

        Just (SomeGameSession (SudokuGameSession sessionId gameMoves isWin)) -> do
            if isWin 
            then do
                status badRequest400
                json $ errorResponse 400 "Game already won." Map.empty
            else do
                req <- jsonData :: ActionM PlaySudokuRequest
                let (col, row, newVal) = move req
                let newMove = safeReplace2D col row newVal $ last gameMoves

                case newMove of
                    Nothing -> do
                        status badRequest400
                        json $ errorResponse 400 "Invalid move." Map.empty
                    Just nm -> do
                        let validMove = isValidSudokuBoard $ Just nm

                        case validMove of
                            Nothing -> do
                                status badRequest400
                                json $ errorResponse 400 "Invalid move." Map.empty

                            Just vm -> do
                                let sudokuWin = isBoardFilled vm
                                let updatedGameMoves = gameMoves ++ [nm]
                                let newGameSession = SudokuGameSession sessionId updatedGameMoves sudokuWin

                                _ <- liftIO $ updateGameSession sessionId newGameSession

                                let responseMsg =
                                        if sudokuWin
                                        then "Game won."
                                        else "Move saved successfully."

                                let response = successResponse 200 responseMsg $ Map.fromList [("board", toJSON newMove)]
                                json response

        Just (SomeGameSession _) -> do
            status badRequest400
            json $ errorResponse 400 "Invalid game session type." Map.empty