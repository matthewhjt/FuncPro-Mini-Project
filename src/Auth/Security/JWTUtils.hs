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
      claims,
      stringOrURI,
      iss,
      exp,
      numericDate
    )
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)

secretKey :: Text
secretKey = "funpro-is-very-fun"

createToken :: Text -> IO Text
createToken username = do
    currentTime <- getPOSIXTime
    let expiry = currentTime + 3600  -- Token expiry (1 hour)
    let payload = mempty { iss = stringOrURI username, Web.JWT.exp = numericDate expiry }
    let token = encodeSigned (hmacSecret secretKey) mempty payload
    return token

verifyToken :: Text -> Maybe (JWT VerifiedJWT)
verifyToken token = decodeAndVerifySignature (toVerify $ hmacSecret secretKey) token
