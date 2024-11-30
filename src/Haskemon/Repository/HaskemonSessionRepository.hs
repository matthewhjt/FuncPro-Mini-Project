module Haskemon.Repository.HaskemonSessionRepository (
    saveGameSession
) where

import Haskemon.Model.HaskemonSessionModel
import Haskemon.Repository.HaskemonRepository (toDoc)
import Database.MongoDB ( Document, ObjectId, Value(..), (=:), insert )
import Lib (runDB)

toDocu :: HaskemonSession -> Document
toDocu session =
    [ "sessionId" =: sessionId session
    , "team1" =: map Haskemon.Repository.HaskemonRepository.toDoc (team1 session)
    , "team2" =: map Haskemon.Repository.HaskemonRepository.toDoc (team2 session)
    , "currentTeam" =: currentTeam session
    ]

saveGameSession :: HaskemonSession -> IO (Either String ())
saveGameSession session = do
    result <- runDB $ insert "haskemonSessions" (toDocu session)
    case result of
        ObjId _ -> return $ Right ()
        _       -> return $ Left "Error saving the session to the database."

