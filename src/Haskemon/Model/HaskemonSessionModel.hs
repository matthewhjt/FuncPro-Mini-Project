{-# LANGUAGE DeriveGeneric #-}

module Haskemon.Model.HaskemonSessionModel
    ( HaskemonSession(..)
    , toJSON
    , fromJSON
    , PlayHaskemonRequest(..)
    ) where

import GHC.Generics (Generic)
import Haskemon.Model.HaskemonModel (HaskemonModel)
import Database.MongoDB (Value, val)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), object, (.=), withObject, (.:), fromJSON)
import Data.Text (unpack)

data HaskemonSession = HaskemonSession
    { sessionId :: Value
    , team1 :: [HaskemonModel]
    , team2 :: [HaskemonModel]
    , winner :: Int
    , currentHaskemon :: Int
    } deriving (Show, Generic)

instance ToJSON HaskemonSession where
    toJSON (HaskemonSession sessionId team1 team2 winner currentHaskemon) = object
        [ "sessionId" .= show sessionId
        , "team1" .= team1
        , "team2" .= team2
        , "winner" .= winner
        , "currentHaskemon" .= currentHaskemon
        ]

instance FromJSON HaskemonSession where
    parseJSON = withObject "HaskemonSession" $ \v -> do
        sessionIdStr <- v .: "sessionId"
        team1 <- v .: "team1"
        team2 <- v .: "team2"
        winner <- v .: "winner"
        currentHaskemon <- v .: "currentHaskemon"
        let sessionId = val (unpack sessionIdStr)
        return $ HaskemonSession sessionId team1 team2 winner currentHaskemon

newtype PlayHaskemonRequest = PlayHaskemonRequest
    {
        move :: HaskemonMove
    }

type HaskemonMove = (Int, Int)

instance FromJSON PlayHaskemonRequest where
    parseJSON = withObject "PlayHaskemonRequest" $ \v -> do
        move <- v .: "move"
        return $ PlayHaskemonRequest move