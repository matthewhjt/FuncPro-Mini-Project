{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty (get, html, pathParam, scotty)

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- pathParam "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
