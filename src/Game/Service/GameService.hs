module Game.Service.GameService (
    getAllGames
) where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Game.Repository.GameRepository (findGames)
import Web.Scotty (json, ActionM)
import Lib (ApiResponse(..))
import qualified Data.Map as Map
import Data.Aeson (toJSON)

getAllGames :: ActionM ()
getAllGames = do
    games <- liftIO findGames
    let response = ApiResponse
            { code = 200
            , success = True
            , message = "Games retrieved successfully."
            , dataFields = Map.fromList
                [
                    ("games", toJSON games)
                ]
            }
    json response
