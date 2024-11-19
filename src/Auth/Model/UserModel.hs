{-# LANGUAGE DeriveGeneric #-}

module Auth.Model.UserModel where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data User = User
  { username :: String
  , password :: String
  } deriving (Show, Eq, Generic)

instance ToJSON User
instance FromJSON User
