{-# LANGUAGE OverloadedStrings #-}

module Auth.Service.AuthService where

import Auth.Repository.UserRepository (isUsernameTaken, addUser, getUser)
import Auth.Model.UserModel (User(..))
import Auth.Security.JWTUtils (createToken)

import Data.Aeson (object, (.=))
import Data.Text (Text, unpack)
import Web.Scotty (json, ActionM)
import Control.Monad.IO.Class (liftIO)
import Lib (ApiResponse(..))

registerUser :: Text -> Text -> ActionM ()
registerUser inputUsername inputPassword = do
    let
        uname = unpack inputUsername
        pwd = unpack inputPassword
    usernameExists <- liftIO $ isUsernameTaken uname
    if usernameExists
        then json (ApiResponse
            { code = 409
            , success = False
            , message = "Username is already taken."
            , dataField = Nothing :: Maybe User
            })
        else do
            let newUser = User uname pwd
            liftIO $ addUser newUser
            json (ApiResponse
                { code = 201
                , success = True
                , message = "User successfully registered."
                , dataField = Just newUser
                })

login :: Text -> Text -> ActionM ()
login inputUsername inputPassword = do
    maybeUser <- liftIO $ getUser (unpack inputUsername)
    case maybeUser of
        Nothing -> json (object ["error" .= ("User not found" :: String)])
        Just user ->
            if password user == unpack inputPassword
                then do
                    token <- liftIO $ createToken inputUsername
                    json (object ["token" .= token])
                else
                    json (object ["error" .= ("Invalid password" :: String)])
