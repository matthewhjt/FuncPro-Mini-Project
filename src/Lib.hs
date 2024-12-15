{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib (runDB, ApiResponse(..), safeCreateObjectId) where

import Database.MongoDB (connect, host, access, master, Action, auth, readHostPort)
import Data.Aeson (object, Key, Value, KeyValue((.=)), ToJSON(toJSON))
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word ()
import Numeric (readHex)
import Data.Bson (ObjectId(Oid))
import Data.Char (isHexDigit)
import Control.Monad ( guard )
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import Control.Exception (throwIO)

getDbCredentials :: IO (String, Text, Text)
getDbCredentials = do
    maybeUrl <- lookupEnv "DB_URL"
    maybeUname <- lookupEnv "DB_USERNAME"
    maybePwd <- lookupEnv "DB_PASSWORD"
    
    case (maybeUrl, maybeUname, maybePwd) of
        (Just url, Just uname, Just pwd) -> 
            return (url, pack uname, pack pwd)
        _ -> throwIO $ userError "Environment variables DB_URL, DB_USERNAME, or DB_PASSWORD are not set."

-- runDB :: Action IO a -> IO a
-- runDB act = do
--     (url, uname, pwd) <- getDbCredentials
--     let hostname = readHostPort url
--     pipe <- connect hostname
--     e <- access pipe master "admin" $ auth uname pwd
--     liftIO $ print e
--     access pipe master "local" act

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