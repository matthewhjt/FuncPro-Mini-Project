module Lib (runDB) where

import Database.MongoDB

runDB :: Action IO a -> IO a
runDB act = do
    pipe <- connect $ host "127.0.0.1"
    access pipe master "local" act