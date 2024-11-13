module Game.Service.GameService (
    getAllGames
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Game.Repository.GameRepository (findGames)
import Web.Scotty (json, ActionM)

getAllGames :: ActionM ()
getAllGames = do
    games <- liftIO findGames
    json games
