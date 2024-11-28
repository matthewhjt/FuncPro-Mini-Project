{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Model.GameSessionModel 
    (
    GameSession(..),
    getGameSessionMoves,
    PlaySudokuRequest(..),
    getPlaySudokuRequestMove
    ) where

import GHC.Generics (Generic)
import Game.Service.GameValidator (Board)
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


getGameSessionMoves :: GameSession -> [Board]
getGameSessionMoves (GameSession _ moves _) = moves

newtype PlaySudokuRequest = PlaySudokuRequest 
    {
    move :: Board
    }

getPlaySudokuRequestMove :: PlaySudokuRequest -> Board
getPlaySudokuRequestMove (PlaySudokuRequest move) = move

instance FromJSON PlaySudokuRequest where
    parseJSON = withObject "PlaySudokuRequest" $ \v -> do
        move <- v .: "move"
        return $ PlaySudokuRequest move