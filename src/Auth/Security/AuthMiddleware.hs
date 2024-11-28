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
    authHeader <- header "Authorization"
    case authHeader of
        Nothing -> unauthorizedResponse
        Just token -> do
            let tokenText = toStrict token
                cleanToken = T.stripPrefix "Bearer " tokenText
            case cleanToken of
                Nothing -> unauthorizedResponse
                Just t -> do
                    verified <- liftIO $ verifyToken t
                    case verified of
                        Nothing -> unauthorizedResponse
                        Just jwt -> do
                            -- Refresh token if necessary
                            refreshedToken <- liftIO $ refreshToken t
                            case refreshedToken of
                                Nothing -> unauthorizedResponse
                                Just newToken -> do
                                    setHeader "X-Refreshed-Token" (fromStrict newToken) -- Add new token to the response
                                    next

unauthorizedResponse :: ActionM ()
unauthorizedResponse = do
    status unauthorized401
    json $ object ["error" .= ("Unauthorized: Invalid or missing token" :: String)]