{-# LANGUAGE OverloadedStrings #-}

module Auth.Security.AuthMiddleware (
    authMiddleware
) where

import Web.Scotty (ActionM, header, json, status)
import Network.HTTP.Types.Status (unauthorized401)
import Auth.Security.JWTUtils (verifyToken)
import Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import Data.Aeson (object, (.=))
import Data.Maybe (isNothing)

authMiddleware :: ActionM () -> ActionM ()
authMiddleware next = do
    authHeader <- header "Authorization"
    case authHeader of
        Nothing -> unauthorizedResponse
        Just token -> do
            let tokenText = toStrict token
                cleanToken = T.stripPrefix "Bearer " tokenText
            case cleanToken of
                Nothing -> unauthorizedResponse
                Just t -> if isNothing (verifyToken t)
                         then unauthorizedResponse
                         else next

unauthorizedResponse :: ActionM ()
unauthorizedResponse = do
    status unauthorized401
    json $ object ["error" .= ("Unauthorized: Invalid or missing token" :: String)]