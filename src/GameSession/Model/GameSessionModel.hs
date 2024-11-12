{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GameSession.Model.GameSessionModel 
    (
    GameSession(..)
    ) 
where

import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data GameSession = GameSession
  { gameSessionId :: UUID,
    gameId :: UUID,
    turnId :: UUID,
    isFirst :: Bool,
    isWin :: Bool
  }
  deriving (Generic, FromJSON, ToJSON)

instance FromRow GameSession where
  fromRow = GameSession <$> field <*> field <*> field <*> field <*> field