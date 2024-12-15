{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Repository.GameSessionRepository (
    createGameSession,
    findGameSessionById,
    updateGameSession,
) where
import GameSession.Model.GameSessionModel 
import Lib (runDB)
import Database.MongoDB
    ( (=:), findOne, insert, Document, Value, Select(select), ObjectId, save)
import qualified Database.MongoDB as M (lookup)
import Game.Service.GameValidator.GameValidator (Board)
import Data.Bson (valMaybe)
import Haskemon.Model.HaskemonModel (HaskemonModel)

createGameSession :: GameSession a -> IO Value
createGameSession gameSession = do
  runDB $ insert "gameSession" (toDoc gameSession)

findGameSessionById :: Maybe ObjectId -> IO (Maybe SomeGameSession)
findGameSessionById sessionId = do
  doc <- runDB $ findOne $ select ["_id" =: valMaybe sessionId] "gameSession"
  return $ doc >>= fromDoc

updateGameSession :: Value -> GameSession a -> IO (Maybe (GameSession a))
updateGameSession sessionId gameSession = do
  let query = select ["_id" =: sessionId] "gameSession"
  let gameSessionDoc = toDoc gameSession
  existingDoc <- runDB $ findOne query
  case existingDoc of
    Nothing -> return Nothing
    Just _ -> do
      _ <- runDB $ save "gameSession" gameSessionDoc
      return $ Just gameSession
  
fromDoc :: Document -> Maybe SomeGameSession
fromDoc doc = case M.lookup "type" doc :: Maybe String of
    Just t | t == "sudoku" -> do
        sessionId <- M.lookup "_id" doc :: Maybe Value
        moves <- M.lookup "moves" doc :: Maybe [Board]
        isWin <- M.lookup "isWin" doc :: Maybe Bool
        return $ SomeGameSession $ SudokuGameSession sessionId moves isWin

    Just t | t == "haskemon" -> do
        sessionId <- M.lookup "_id" doc :: Maybe Value
        team <- M.lookup "team" doc :: Maybe [HaskemonModel]
        enemy <- M.lookup "enemy" doc :: Maybe [HaskemonModel]
        level <- M.lookup "level" doc :: Maybe Int
        return $ SomeGameSession $ HaskemonGameSession sessionId team enemy level

    _ -> Nothing


toDoc :: GameSession a -> Document
toDoc session = case session of
    SudokuGameSession sessionId moves isWin -> 
        [ "_id" =: sessionId
        , "type" =: ("sudoku" :: String)
        , "moves" =: moves
        , "isWin" =: isWin
        ]
    HaskemonGameSession sessionId team enemy level -> 
        [ "_id" =: sessionId
        , "type" =: ("haskemon" :: String)
        , "team" =: team
        , "enemy" =: enemy
        , "level" =: level
        ]