{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Model.GameSessionModel (GameSession(..)) where
import GHC.Generics (Generic)
import Game.Service.GameValidator (Board)
import Data.Aeson (ToJSON (toJSON), object, (.=))
import Database.MongoDB (Value)

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

-- instance ToJSON GameSession where
--     toJSON (GameSession gameSessionId moves isWin) = object
--         [ "gameSession" .= object
--             [ "gameSessionId" .= show gameSessionId
--             , "moves" .= moves
--             , "isWin" .= isWin
--             ]
--         ]