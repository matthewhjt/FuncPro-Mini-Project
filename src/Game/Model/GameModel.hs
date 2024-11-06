module Game.Model.GameModel
  ( GameType (..),
    Game (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, (.:), (.=))
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (ConversionFailed), returnError)
import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

data GameType = Board | TurnBased

instance FromField GameType where
  fromField f mdata = 
    case mdata of
      Just "BOARD"    -> pure Board
      Just "TURN_BASED" -> pure TurnBased
      _                -> returnError ConversionFailed f "Invalid GameType"

data Game = Game
  { gameId :: UUID,
    gameType :: GameType,
    gameName :: String
  }

instance FromRow Game where
  fromRow = Game <$> field <*> field <*> field

instance FromJSON GameType where
  parseJSON (String "BOARD") = pure Board
  parseJSON (String "TURN_BASED") = pure TurnBased
  parseJSON _ = fail "Invalid GameType"

instance ToJSON GameType where
  toJSON Board = String "BOARD"
  toJSON TurnBased = String "TURN_BASED"

instance FromJSON Game where
  parseJSON = withObject "Game" $ \v ->
    Game
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "name"

instance ToJSON Game where
  toJSON (Game id' type' name') =
    object
      [ "id" .= id',
        "type" .= type',
        "name" .= name'
      ]
