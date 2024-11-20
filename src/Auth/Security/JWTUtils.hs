{-# LANGUAGE OverloadedStrings #-}

module Auth.Security.JWTUtils (
    createToken,
    verifyToken
) where

import Web.JWT
    ( encodeSigned,
      decodeAndVerifySignature,
      hmacSecret,
      toVerify,
      JWT,
      VerifiedJWT,
      stringOrURI,
      iss,
      exp,
      numericDate
    )
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import Control.Exception (throwIO)

secretKey :: IO Text
secretKey = do
    maybeKey <- lookupEnv "SECRET_KEY"
    case maybeKey of
        Just key -> return $ pack key
        Nothing -> throwIO $ userError "Environment variable SECRET_KEY is not set."

createToken :: Text -> IO Text
createToken username = do
    currentTime <- getPOSIXTime
    let expiry = currentTime + 3600  -- Token expiry (1 hour)
    secretKeyValue <- secretKey
    let payload = mempty { iss = stringOrURI username, Web.JWT.exp = numericDate expiry }
    let token = encodeSigned (hmacSecret secretKeyValue) mempty payload
    return token

verifyToken :: Text -> IO (Maybe (JWT VerifiedJWT))
verifyToken token = do
    key <- secretKey
    return $ decodeAndVerifySignature (toVerify $ hmacSecret key) token
