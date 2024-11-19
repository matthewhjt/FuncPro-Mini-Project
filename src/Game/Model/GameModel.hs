{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Game.Model.GameModel (Game(..)) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import GHC.Generics (Generic)
import Database.MongoDB (ObjectId)

data Game = Game
    { gameId   :: ObjectId
    , name     :: String
    , gameType :: String
    } deriving (Show, Generic)

instance ToJSON Game where
    toJSON (Game gameId name gameType) = object
        [ "gameId" .= show gameId
        , "name" .= name
        , "gameType" .= gameType
        ]
    