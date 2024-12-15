{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GameSession.Model.GameSessionModel
    ( GameSession(..)
    , SessionType(..)
    , SomeGameSession(..)
    , PlaySudokuRequest(..)
    ) where

import Game.Service.GameValidator.GameValidator (Board)
import Data.Aeson 
    ( ToJSON(toJSON), FromJSON(parseJSON)
    , object, (.=), (.:), withObject
    )
import Database.MongoDB (Value, val)
import Data.Text (unpack, pack)
import Haskemon.Model.HaskemonModel (HaskemonModel)

data SessionType = SudokuSession | HaskemonSession

data GameSession (a :: SessionType) where
    SudokuGameSession :: {
        sudokuSessionId :: Value
      , moves :: [Board]
      , isWin :: Bool
    } -> GameSession 'SudokuSession

    HaskemonGameSession :: {
        haskemonSessionId :: Value
      , team :: [HaskemonModel]
      , enemy :: [HaskemonModel]
      , level :: Int
    } -> GameSession 'HaskemonSession

data SomeGameSession where
    SomeGameSession :: GameSession a -> SomeGameSession

instance ToJSON (GameSession 'SudokuSession) where
    toJSON (SudokuGameSession sudokuSessionId moves isWin) = object
        [ "gameSessionId" .= show sudokuSessionId
        , "moves" .= moves
        , "isWin" .= isWin
        ]

instance ToJSON (GameSession 'HaskemonSession) where
    toJSON (HaskemonGameSession haskemonSessionId team enemy level) = object
        [ "gameSessionId" .= show haskemonSessionId
        , "team" .= team
        , "enemy" .= enemy
        , "level" .= level
        ]

instance FromJSON (GameSession 'SudokuSession) where
    parseJSON = withObject "SudokuGameSession" $ \v -> do
        gameSessionIdStr <- v .: "gameSessionId"
        moves <- v .: "moves"
        isWin <- v .: "isWin"
        let gameSessionId = val (unpack (pack gameSessionIdStr))
        return $ SudokuGameSession gameSessionId moves isWin

instance FromJSON (GameSession 'HaskemonSession) where
    parseJSON = withObject "HaskemonGameSession" $ \v -> do
        gameSessionIdStr <- v .: "gameSessionId"
        team <- v .: "team"
        enemy <- v .: "enemy"
        level <- v .: "level"
        let gameSessionId = val (unpack (pack gameSessionIdStr))
        return $ HaskemonGameSession gameSessionId team enemy level

instance ToJSON SomeGameSession where
    toJSON (SomeGameSession (SudokuGameSession sudokuSessionId moves isWin)) = object
        [ "sessionType" .= ("SudokuSession" :: String)
        , "gameSessionId" .= show sudokuSessionId
        , "moves" .= moves
        , "isWin" .= isWin
        ]
    toJSON (SomeGameSession (HaskemonGameSession haskemonSessionId team enemy level)) = object
        [ "sessionType" .= ("HaskemonSession" :: String)
        , "gameSessionId" .= show haskemonSessionId
        , "team" .= team
        , "enemy" .= enemy
        , "level" .= level
        ]


newtype PlaySudokuRequest = PlaySudokuRequest
    {
        move :: SudokuMove
    }

type SudokuMove = (Int, Int, Int)

instance FromJSON PlaySudokuRequest where
    parseJSON = withObject "PlaySudokuRequest" $ \v -> do
        move <- v .: "move"
        return $ PlaySudokuRequest move

-- newtype CreateHaskemonSessionRequest = CreateHaskemonSessionRequest 
--     {
--     team :: [HaskemonModel]
--     }

-- instance FromJSON CreateHaskemonSessionRequest where
--     parseJSON = withObject "CreateHaskemonSessionRequest" $ \v -> do
--         team <- v .: "team"
--         return $ CreateHaskemonSessionRequest team