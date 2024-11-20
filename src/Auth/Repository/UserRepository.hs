{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Auth.Repository.UserRepository (
    addUser,
    isUsernameTaken,
    getUser
) where

import Database.MongoDB
    ( Document, ObjectId, Value (..), findOne, insert, select, (=:) )
import qualified Database.MongoDB as M (lookup)
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (isJust)
import Lib (runDB)
import Auth.Model.UserModel (User(..))

addUser :: User -> IO (Either String ObjectId)
addUser user = do
    let doc = toDoc user
    result <- runDB $ insert "users" doc
    case result of
        ObjId oid -> return $ Right oid
        _         -> return $ Left "Failed to retrieve ObjectId after insert."

isUsernameTaken :: String -> IO Bool
isUsernameTaken uname = do
    let selector = select ["username" =: uname] "users"
    result <- runDB $ findOne selector
    return $ isJust result

getUser :: String -> IO (Maybe User)
getUser uname = do
    let selector = select ["username" =: uname] "users"
    doc <- runDB $ findOne selector
    return $ fromDoc =<< doc

toDoc :: User -> Document
toDoc (User uname pwd) = ["username" =: uname, "password" =: pwd]

fromDoc :: Document -> Maybe User
fromDoc = runReaderT $ do
    uname <- getUsername
    pwd <- getPassword
    return $ User uname pwd

getUsername :: ReaderT Document Maybe String
getUsername = ReaderT $ M.lookup "username"

getPassword :: ReaderT Document Maybe String
getPassword = ReaderT $ M.lookup "password"
