{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib (runDB, ApiResponse(..)) where

import Database.MongoDB (connect, host, access, master, Action)
import Data.Aeson (object, Key, Value, KeyValue((.=)), ToJSON(toJSON))
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map

runDB :: Action IO a -> IO a
runDB act = do
    pipe <- connect $ host "127.0.0.1"
    access pipe master "local" act

data ApiResponse = ApiResponse
    { code    :: Int
    , success :: Bool
    , message :: String
    , dataFields :: Map Key Value
    } deriving (Show, Generic)

instance ToJSON ApiResponse where
    toJSON (ApiResponse code success message dataFields) = 
        object $ 
            [ "code" .= code
            , "success" .= success
            , "message" .= message
            ] ++ Map.toList dataFields