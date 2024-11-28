{-# OPTIONS_GHC -Wno-name-shadowing #-}
module GameSession.Repository.GameSessionRepository (
    createGameSession,
    findGameSessionById,
    updateGameSession,
    getMoves
) where
import GameSession.Model.GameSessionModel (GameSession (..))
import Lib (runDB)
import Database.MongoDB
    ( (=:), findOne, insert, Document, Value, Select(select), ObjectId, save)
import qualified Database.MongoDB as M (lookup)
import Control.Monad.Reader (ReaderT(..))
import Game.Service.GameValidator (Board)
import Data.Bson (valMaybe)


createGameSession :: Board -> IO Value
createGameSession board = do
  runDB $ insert "gameSession" ["moves" =: [board], "isWin" =: False]

findGameSessionById :: Maybe ObjectId -> IO (Maybe GameSession)
findGameSessionById sessionId = do
  doc <- runDB $ findOne $ select ["_id" =: valMaybe sessionId] "gameSession"
  return $ doc >>= fromDoc

updateGameSession :: Value -> GameSession -> IO (Maybe GameSession)
updateGameSession sessionId gameSession = do
  let query = select ["_id" =: sessionId] "gameSession"
  let gameSessionDoc = toDoc gameSession
  existingDoc <- runDB $ findOne query
  case existingDoc of
    Nothing -> return Nothing
    Just _ -> do
      _ <- runDB $ save "gameSession" gameSessionDoc
      return $ Just gameSession
  
toDoc :: GameSession -> Document
toDoc s = ["_id" =: gameSessionId s, "moves" =: moves s, "isWin" =: isWin s]

fromDoc :: Document -> Maybe GameSession
fromDoc = runReaderT $ do
  gameSessionId <- getGameSessionId
  moves <- getMoves
  GameSession gameSessionId moves <$> getIsWin

getGameSessionId :: ReaderT Document Maybe Value
getGameSessionId = ReaderT $ M.lookup "_id"

getMoves :: ReaderT Document Maybe [Board]
getMoves = ReaderT $ M.lookup "moves"

getIsWin :: ReaderT Document Maybe Bool
getIsWin = ReaderT $ M.lookup "isWin"