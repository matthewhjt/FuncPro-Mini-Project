{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty ( get, scotty, ScottyM )
import Game.Service.GameService (getAllGames)

funpro :: ScottyM()
funpro = do
    get "/games" $ getAllGames

main :: IO ()
main = do
    scotty 3000 funpro