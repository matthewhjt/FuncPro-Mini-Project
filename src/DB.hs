{-# LANGUAGE ImportQualifiedPost #-}

module DB (getConnection) where

import Database.PostgreSQL.Simple
  ( ConnectInfo
      ( connectDatabase,
        connectHost,
        connectPassword,
        connectUser,
        connectPort
      ),
    Connection,
    connect,
    defaultConnectInfo,
  )
import Hardcoded qualified

getConnection :: IO Connection
getConnection =
  connect $
    defaultConnectInfo
      { connectHost = Hardcoded.host,
        connectDatabase = Hardcoded.database,
        connectUser = Hardcoded.user,
        connectPassword = Hardcoded.password,
        connectPort = Hardcoded.portNumber
      }