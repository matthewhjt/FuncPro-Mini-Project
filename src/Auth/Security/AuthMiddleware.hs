{-# LANGUAGE OverloadedStrings #-}

module Auth.Security.AuthMiddleware (
    authMiddleware
) where

import Web.Scotty (ActionM, header, json, status, setHeader)
import Network.HTTP.Types.Status (unauthorized401)
import Auth.Security.JWTUtils (verifyToken, refreshToken)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text as T

authMiddleware :: ActionM () -> ActionM ()
authMiddleware next = do
    header "Authorization" >>= maybe unauthorizedResponse handleToken
  where
    handleToken token = do
        let tokenText = toStrict token
        maybe unauthorizedResponse verifyTokenWrapper (T.stripPrefix "Bearer " tokenText)

    verifyTokenWrapper t = do
        liftIO (verifyToken t) >>= maybe unauthorizedResponse (handleJWT t)

    handleJWT t jwt = do
        let username = extractUsername jwt
        maybe unauthorizedResponse (refreshAndContinue t) username

    refreshAndContinue t usr = do
        liftIO (refreshToken t) >>= maybe unauthorizedResponse (proceed usr)

    proceed usr newToken = do
        setHeader "X-Refreshed-Token" (fromStrict newToken)
        next (T.unpack usr)

extractUsername :: Web.JWT.JWT Web.JWT.VerifiedJWT -> Maybe T.Text
extractUsername jwt = fmap (T.pack . show) (iss (claims jwt))

unauthorizedResponse :: ActionM ()
unauthorizedResponse = do
    status unauthorized401
    json $ object ["error" .= ("Unauthorized: Invalid or missing token" :: String)]