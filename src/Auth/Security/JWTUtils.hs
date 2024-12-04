{-# LANGUAGE OverloadedStrings #-}

module Auth.Security.JWTUtils (
    createToken,
    verifyToken,
    refreshToken
) where

import Web.JWT
    ( encodeSigned,
      decodeAndVerifySignature,
      hmacSecret,
      toVerify,
      JWT,
      VerifiedJWT,
      claims,
      stringOrURI,
      iss,
      exp,
      numericDate
    )
import Data.Text (Text, pack, unpack)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (lookupEnv)
import Control.Exception (throwIO)
import Data.Maybe (fromMaybe)

secretKey :: IO Text
secretKey = do
    maybeKey <- lookupEnv "SECRET_KEY"
    case maybeKey of
        Just key -> return $ pack key
        Nothing -> throwIO $ userError "Environment variable SECRET_KEY is not set."

createToken :: Text -> IO Text
createToken username = do
    currentTime <- getPOSIXTime
    let expiry = currentTime + 7200  -- Token expiry (2 hour)
    secretKeyValue <- secretKey
    let payload = mempty { iss = stringOrURI username, Web.JWT.exp = numericDate expiry }
    let token = encodeSigned (hmacSecret secretKeyValue) mempty payload
    return token

verifyToken :: Text -> IO (Maybe (JWT VerifiedJWT))
verifyToken token = do
    key <- secretKey
    return $ decodeAndVerifySignature (toVerify $ hmacSecret key) token

refreshToken :: Text -> IO (Maybe Text)
refreshToken token = do
    key <- secretKey
    case decodeAndVerifySignature (toVerify $ hmacSecret key) token of
        Nothing -> return Nothing
        Just jwt -> do
            let claimsSet = claims jwt
                username = fmap (pack . show) (iss claimsSet)
            case username of
                Nothing -> return Nothing
                Just usr -> do
                    newToken <- createToken usr
                    return (Just newToken)
