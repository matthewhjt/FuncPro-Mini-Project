{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Game.Repository.GameRepository (
    findGames
) where

import Database.MongoDB
    ( ObjectId, find, rest, Document, Select(select) )
import qualified Database.MongoDB as M (lookup)
import Game.Model.GameModel (Game(Game))
import Control.Monad.Reader (ReaderT(..))
import Data.Maybe (catMaybes)
import Lib (runDB)

findGames :: IO [Game]
findGames =
    let docs = find (select [] "games") >>= rest
    in fmap fromDocs (runDB docs)

fromDocs :: [Document] -> [Game]
fromDocs ds =
  let mp = fmap fromDoc ds
  in catMaybes mp

fromDoc :: Document -> Maybe Game
fromDoc = runReaderT $ do
    gameId <- getGameId
    name <- getName
    Game gameId name <$> getGameType

getGameId :: ReaderT Document Maybe ObjectId
getGameId = ReaderT $ M.lookup "_id"

getName :: ReaderT Document Maybe String
getName = ReaderT $ M.lookup "name"

getGameType :: ReaderT Document Maybe String
getGameType = ReaderT $ M.lookup "gameType"