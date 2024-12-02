{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Model.GameSessionModel 
    (
    GameSession(..),
    PlaySudokuRequest(..),
    ) where

import GHC.Generics (Generic)
import Game.Service.GameValidator.GameValidator (Board)
import Data.Aeson (ToJSON (toJSON), object, (.=), FromJSON (parseJSON), withObject, (.:))
import Database.MongoDB (Value, val)
import Data.Text (unpack)

data GameSession = GameSession
    { gameSessionId :: Value
    , moves :: [Board]
    , isWin :: Bool
    } deriving (Show, Generic)

instance ToJSON GameSession where
    toJSON (GameSession gameSessionId moves isWin) = object
        [ "gameSessionId" .= show gameSessionId
        , "moves" .= moves
        , "isWin" .= isWin
        ]

instance FromJSON GameSession where
    parseJSON = withObject "GameSession" $ \v -> do
        gameSessionIdStr <- v .: "gameSessionId"
        moves <- v .: "moves"
        isWin <- v .: "isWin"
        let gameSessionId = val (unpack gameSessionIdStr)
        return $ GameSession gameSessionId moves isWin

newtype PlaySudokuRequest = PlaySudokuRequest 
    {
    move :: Board
    }

instance FromJSON PlaySudokuRequest where
    parseJSON = withObject "PlaySudokuRequest" $ \v -> do
        move <- v .: "move"
        return $ PlaySudokuRequest move