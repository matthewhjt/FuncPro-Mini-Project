module API.ApiResponse (ApiResponse(..)) where

import Data.Aeson (ToJSON (..), object, (.=))

data ApiResponse a = ApiResponse
  { statusCode :: Int,
    message :: String,
    dataItems :: a
  }

instance ToJSON a => ToJSON (ApiResponse a) where
  toJSON (ApiResponse code msg items) =
    object ["statusCode" .= code, "message" .= msg, "data" .= items]