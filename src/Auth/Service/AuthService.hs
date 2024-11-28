{-# LANGUAGE OverloadedStrings #-}

module Auth.Service.AuthService where

import Auth.Repository.UserRepository (isUsernameTaken, addUser, getUser)
import Auth.Model.UserModel (User(..))
import Auth.Security.JWTUtils (createToken)

import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Aeson (object, (.=))
import Data.Text (Text, unpack, pack)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Web.Scotty (json, ActionM)
import Control.Monad.IO.Class (liftIO)
import Lib (ApiResponse(..))

import qualified Data.Map as Map

hashPassword :: Text -> IO Text
hashPassword password = do
    let passwordBytes = encodeUtf8 password
    hashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy passwordBytes
    return $ decodeUtf8 $ fromMaybe "" hashed

registerUser :: Text -> Text -> ActionM ()
registerUser inputUsername inputPassword = do
    let uname = unpack inputUsername
    usernameExists <- liftIO $ isUsernameTaken uname
    if usernameExists
        then json (ApiResponse
            { code = 409
            , success = False
            , message = "Username is already taken."
            , dataFields = Map.empty
            })
        else do
            hashedPwd <- liftIO $ hashPassword inputPassword
            let newUser = User uname (unpack hashedPwd)
            liftIO $ addUser newUser
            json (ApiResponse
                { code = 201
                , success = True
                , message = "User successfully registered."
                , dataFields = Map.empty
                })

login :: Text -> Text -> ActionM ()
login inputUsername inputPassword = do
    maybeUser <- liftIO $ getUser (unpack inputUsername)
    case maybeUser of
        Nothing -> json (object ["error" .= ("User not found" :: String)])
        Just user -> do
            let hashedPwd = encodeUtf8 $ pack $ password user
            let inputPwd = encodeUtf8 inputPassword
            if validatePassword hashedPwd inputPwd
                then do
                    token <- liftIO $ createToken inputUsername
                    json (object ["token" .= token])
                else
                    json (object ["error" .= ("Invalid password" :: String)])
