module GameSession.Controller.GameSessionController 
  (
    gameSessionRoutes
  )
where

import Database.PostgreSQL.Simple (Connection)
import Web.Scotty (get, scotty)
import GameSession.Service.GameSessionService (createSudoku)

gameSessionRoutes :: Connection -> IO ()
gameSessionRoutes conn = scotty 3000 $ do
  get "/api/newGame/sudoku" $ createSudoku conn