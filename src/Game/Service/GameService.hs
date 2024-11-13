module Game.Service.GameService (
    getAllGames
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Game.Repository.GameRepository (findGames)
import Web.Scotty (json, ActionM)
import Lib (ApiResponse(..))

getAllGames :: ActionM ()
getAllGames = do
    games <- liftIO findGames
    let response = ApiResponse
            { code = 200
            , success = True
            , message = "Games retrieved successfully."
            , dataField = games
            }
    json response
