module Haskemon.Model.HaskemonSessionModel
    ( HaskemonSession
    , HaskemonSession (..)
    )
    where

import Haskemon.Model.HaskemonModel
import Database.MongoDB (ObjectId)

data HaskemonSession = HaskemonSession
    { sessionId :: ObjectId
    , team1 :: [HaskemonModel]
    , team2 :: [HaskemonModel]
    , currentTeam :: Int
    } deriving (Show)
