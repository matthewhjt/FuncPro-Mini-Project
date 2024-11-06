module Game.Service.GameService
  ( getGames,
  )
where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection, query_)
import Game.Model.GameModel (Game (..))
import Web.Scotty (ActionM)
import qualified Web.Scotty as S
import API.ApiResponse (ApiResponse(..))

getGames :: Connection -> ActionM ()
getGames conn = do
  games <- liftIO $ query_ conn "SELECT * FROM game" :: ActionM [Game]
  let response = ApiResponse
        { statusCode = 200,
          message = "Games retrieved successfully",
          dataItems = games
        }
  S.json response