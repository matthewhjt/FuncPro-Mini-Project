{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib (runDB, ApiResponse(..), safeCreateObjectId) where

import Database.MongoDB (connect, host, access, master, Action)
import Data.Aeson (object, Key, Value, KeyValue((.=)), ToJSON(toJSON))
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word ()
import Numeric (readHex)
import Data.Bson (ObjectId(Oid))
import Data.Char (isHexDigit)
import Control.Monad ( guard )

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

safeCreateObjectId :: String -> Maybe ObjectId
safeCreateObjectId hexStr = do
    guard (length hexStr == 24)
    guard (all isHexDigit hexStr)
    
    let (timeHex, machineHex) = splitAt 8 hexStr
    time <- parseWord32 timeHex
    machine <- parseWord64 machineHex
    
    return $ Oid time machine
  where
    parseWord32 hex = 
        case readHex hex of
            [(value, "")] -> 
                if value <= maxBound 
                then Just value 
                else Nothing
            _ -> Nothing
    
    parseWord64 hex = 
        case readHex hex of
            [(value, "")] -> 
                if value <= maxBound 
                then Just value 
                else Nothing
            _ -> Nothing