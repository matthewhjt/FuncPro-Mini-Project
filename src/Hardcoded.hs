module Hardcoded (host, port, portNumber, database, user, password, connectionString) where

import Data.String (IsString, fromString)

host :: (IsString s) => s
host = "127.0.0.1" -- "localhost"

port :: (IsString s) => s
port = "5433"

portNumber :: (Integral i) => i
portNumber = 5433

database :: (IsString s) => s
database = "funpro"

user :: (IsString s) => s
user = "postgres"

password :: (IsString s) => s
password = "1234"

connectionString :: (IsString s) => s
connectionString = stringify [("host", host), ("port", port), ("user", user), ("dbname", database), ("password", password)]
  where
    stringify = fromString . unwords . map pair
    pair (key, value) = key <> "=" <> value
