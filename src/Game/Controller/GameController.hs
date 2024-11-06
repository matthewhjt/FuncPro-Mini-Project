module Game.Controller.GameController
  ( routes,
  )
where

import Database.PostgreSQL.Simple (Connection)
import Game.Service.GameService (getGames)
import Web.Scotty (get, scotty, html)

routes :: Connection -> IO ()
routes conn = scotty 3000 $ do
  get "/" $ do
    html "Hello, world!"
  get "/api/game/" $ getGames conn