module Main (main) where

import Game.Controller.GameController (routes)
import DB (getConnection)

main :: IO ()
main = do
  conn <- getConnection

  routes conn