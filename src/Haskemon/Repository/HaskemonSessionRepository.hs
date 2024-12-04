module Haskemon.Repository.HaskemonSessionRepository (
    -- saveGameSession
    createHaskemonSession,
    findHaskemonSessionById,
    updateHaskemonSession
) where

import Haskemon.Model.HaskemonModel
import Haskemon.Model.HaskemonSessionModel
import qualified Haskemon.Repository.HaskemonRepository as HaskemonRepo
import Lib (runDB)
import Database.MongoDB
    ( (=:), findOne, insert, Document, Value, Value(..), Select(select), ObjectId, save)
import qualified Database.MongoDB as M (lookup)
import Control.Monad.Reader (ReaderT(..))
import Data.Bson (valMaybe)

createHaskemonSession :: [HaskemonModel] -> [HaskemonModel] -> IO Value
createHaskemonSession team1 team2 = do
  runDB $ insert "haskemonSession" 
    [ "team1" =: map HaskemonRepo.toDoc team1
    , "team2" =: map HaskemonRepo.toDoc team2
    , "winner" =: (0 :: Int)
    , "currentHaskemon" =: (0 :: Int)
    ]

findHaskemonSessionById :: Maybe ObjectId -> IO (Maybe HaskemonSession)
findHaskemonSessionById sessionId = do
  doc <- runDB $ findOne $ select ["_id" =: valMaybe sessionId] "haskemonSession"
  return $ doc >>= fromDoc

updateHaskemonSession :: Value -> HaskemonSession -> IO (Maybe HaskemonSession)
updateHaskemonSession sessionId haskemonSession = do
  let query = select ["_id" =: sessionId] "haskemonSession"
  let haskemonSessionDoc = toDoc haskemonSession
  existingDoc <- runDB $ findOne query
  case existingDoc of
    Nothing -> return Nothing
    Just _ -> do
      _ <- runDB $ save "haskemonSession" haskemonSessionDoc
      return $ Just haskemonSession

toDoc :: HaskemonSession -> Document
toDoc s = 
  [ "_id" =: sessionId s
  , "team1" =: map HaskemonRepo.toDoc (team1 s)
  , "team2" =: map HaskemonRepo.toDoc (team2 s)
  , "winner" =: winner s
  , "currentHaskemon" =: currentHaskemon s
  ]

fromDoc :: Document -> Maybe HaskemonSession
fromDoc doc = runReaderT (do
  sessionId <- getSessionId
  team1 <- getTeam1
  team2 <- getTeam2
  winner <- getWinner
  currentHaskemon <- getCurrentHaskemon
  return $ HaskemonSession sessionId team1 team2 winner currentHaskemon) doc

getSessionId :: ReaderT Document Maybe Value
getSessionId = ReaderT $ M.lookup "_id"

getTeam1 :: ReaderT Document Maybe [HaskemonModel]
getTeam1 = ReaderT $ \doc -> do
  team1Docs <- M.lookup "team1" doc
  mapM HaskemonRepo.fromDoc team1Docs

getTeam2 :: ReaderT Document Maybe [HaskemonModel]
getTeam2 = ReaderT $ \doc -> do
  team2Docs <- M.lookup "team2" doc
  mapM HaskemonRepo.fromDoc team2Docs

getWinner :: ReaderT Document Maybe Int
getWinner = ReaderT $ M.lookup "winner"

getCurrentHaskemon :: ReaderT Document Maybe Int
getCurrentHaskemon = ReaderT $ M.lookup "currentHaskemon"
