module Main (main) where

import Game.Controller.GameController (gameRoutes)
import GameSession.Controller.GameSessionController (gameSessionRoutes)
import DB (getConnection)

main :: IO ()
main = do
  conn <- getConnection

  gameRoutes conn
  -- gameSessionRoutes conn
