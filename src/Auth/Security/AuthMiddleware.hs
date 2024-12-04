{-# LANGUAGE OverloadedStrings #-}

module Auth.Security.AuthMiddleware (
    authMiddleware
) where

import Web.Scotty (ActionM, header, json, status, setHeader)
import Network.HTTP.Types.Status (unauthorized401)
import Auth.Security.JWTUtils (verifyToken, refreshToken)
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as T
import Data.Aeson (object, (.=))
import Control.Monad.IO.Class (liftIO)
import Web.JWT (claims, iss, VerifiedJWT, JWT)

authMiddleware :: (String -> ActionM ()) -> ActionM ()
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
                            let username = extractUsername jwt
                            case username of
                                Nothing -> unauthorizedResponse
                                Just usr -> do
                                    refreshedToken <- liftIO $ refreshToken t
                                    case refreshedToken of
                                        Nothing -> unauthorizedResponse
                                        Just newToken -> do
                                            setHeader "X-Refreshed-Token" (fromStrict newToken)
                                            next (T.unpack usr)

extractUsername :: Web.JWT.JWT Web.JWT.VerifiedJWT -> Maybe T.Text
extractUsername jwt = fmap (T.pack . show) (iss (claims jwt))

unauthorizedResponse :: ActionM ()
unauthorizedResponse = do
    status unauthorized401
    json $ object ["error" .= ("Unauthorized: Invalid or missing token" :: String)]