module Game.Controller.GameController
  ( gameRoutes,
  )
where

import Database.PostgreSQL.Simple (Connection)
import Game.Service.GameService (getGames)
import Web.Scotty (get, scotty, html)
import GameSession.Service.GameSessionService (createSudoku)

gameRoutes :: Connection -> IO ()
gameRoutes conn = scotty 3000 $ do
  get "/" $ do
    html "Hello, world!"
  get "/api/game/" $ getGames conn
  get "/api/newGame/sudoku" $ createSudoku conn