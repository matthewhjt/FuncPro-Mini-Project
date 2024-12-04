{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Haskemon.Repository.HaskemonRepository (
    saveHaskemon,
    getHaskemonByName,
    getHaskemonsByIds,
    toDoc,
    fromDoc,
) where

import Database.MongoDB
    ( Document, ObjectId, Value(..), Query, Label, insert, findOne, select, (=:), Value, save, find, rest )
import qualified Database.MongoDB as M (lookup)
import Control.Monad.Reader (ReaderT(..))
import Lib (runDB)
import Data.Bson (val, Val)
import qualified Data.Text as T
import Haskemon.Model.HaskemonModel (HaskemonModel(..), HaskemonStats(..), Element(..), parseElement)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)

saveHaskemon :: HaskemonModel -> IO (Either String ObjectId)
saveHaskemon haskemon = do
    let doc = toDoc haskemon
    result <- runDB $ insert "haskemons" doc
    case result of
        ObjId oid -> return $ Right oid
        _         -> return $ Left "Failed to retrieve ObjectId after insert."

getHaskemonByName :: String -> IO (Maybe HaskemonModel)
getHaskemonByName name = do
    let selector = select ["name" =: name] "haskemons"
    doc <- runDB $ findOne selector
    return $ doc >>= fromDoc

getHaskemonsByIds :: [ObjectId] -> IO [HaskemonModel]
getHaskemonsByIds ids = do
    cursor <- runDB $ find (select ["_id" =: ["$in" =: ids]] "haskemons")
    documents <- runDB $ rest cursor
    return $ mapMaybe fromDoc documents

toDoc :: HaskemonModel -> Document
toDoc (HaskemonModel name hp mana element stats username) =
    [ "name" =: name
    , "healthPoint" =: hp
    , "mana" =: mana
    , "element" =: show element
    , "attack" =: attack stats
    , "defense" =: defense stats
    , "username" =: username
    ]

fromDoc :: Document -> Maybe HaskemonModel
fromDoc = runReaderT $ do
    name <- getField "name"
    hp <- getField "healthPoint"
    mana <- getField "mana"
    elementStr <- getField "element"
    atk <- getField "attack"
    def <- getField "defense"
    username <- getField "username"
    element <- lift $ parseElement elementStr
    let stats = HaskemonStats atk def
    return $ HaskemonModel name hp mana element stats username

getField :: (Val a) => String -> ReaderT Document Maybe a
getField key = ReaderT $ M.lookup (T.pack key)
