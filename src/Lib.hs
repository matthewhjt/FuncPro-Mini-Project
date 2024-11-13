{-# LANGUAGE DeriveGeneric #-}
module Lib (runDB, ApiResponse(..)) where

import Database.MongoDB ( connect, host, access, master, Action )
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

runDB :: Action IO a -> IO a
runDB act = do
    pipe <- connect $ host "127.0.0.1"
    access pipe master "local" act

data ApiResponse a = ApiResponse
    { code    :: Int
    , success :: Bool
    , message :: String
    , dataField :: a
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (ApiResponse a)