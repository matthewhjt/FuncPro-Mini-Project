module GameSession.Service.GameSessionService 
  (
    createSudoku
  )
where

import GameSession.Service.Generate (generateSudoku)
import API.ApiResponse (ApiResponse(..))
import Web.Scotty (ActionM)
import Database.PostgreSQL.Simple (Connection, execute)
import GameSession.Model.GameSessionModel (GameSession(GameSession))
import qualified Web.Scotty as S
import Data.Aeson (encode)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Aeson.Types (object)
import Data.Aeson ((.=))

createSudoku :: Connection -> ActionM ()
createSudoku conn = do
  sudoku <- liftIO $ generateSudoku 10
  let sudokuJson = BS.unpack (encode sudoku)
  let result =
        execute
          conn
          "INSERT INTO gamesession (game_id, turn_id, is_first, is_win, board) VALUES (?, ?, ?, ?, ?)"
          ("b1b9efb4-e67e-4738-b022-871b38b8eadf"::String, "550e8400-e29b-41d4-a716-446655440002"::String, True::Bool, False::Bool, sudokuJson :: String)
  n <- liftIO result
  if n > 0
    then do
      -- status status201
      S.json $ object ["message" .= ("Product created" :: String)]
    else do
      -- status status400
      S.json $ object ["error" .= ("Product not created" :: String)]
  -- let response = ApiResponse
  --       { statusCode = 200,
  --         message = "Sudoku created successfully",
  --         dataItems = sudoku
  --       }
    -- S.json response