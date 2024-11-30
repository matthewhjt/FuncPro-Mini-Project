module Haskemon.Service.HaskemonService
    ( createHaskemonForUser
    ) where

import Haskemon.Model.HaskemonModel
import Haskemon.Model.HaskemonSessionModel
import Haskemon.Repository.HaskemonRepository (saveHaskemon, getHaskemonsByIds)
import Haskemon.Repository.HaskemonSessionRepository (saveGameSession)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Bson (ObjectId)
import Data.Text (Text)
import Lib (ApiResponse(..))
import qualified Data.Map as Map

createGameSession :: String -> [ObjectId] -> [ObjectId] -> IO (Either String HaskemonSession)
createGameSession username team1Ids team2Ids = do
    team1Haskemons <- getHaskemonsByIds team1Ids
    team2Haskemons <- getHaskemonsByIds team2Ids
    
    if length team1Haskemons > 3 || length team2Haskemons > 3
        then return $ Left "Teams can have a maximum of 3 Haskemons each."
        else do
            let newSession = HaskemonSession
                    { sessionId = generateNewObjectId 
                    , team1 = team1Haskemons
                    , team2 = team2Haskemons
                    , currentTeam = 1
                    }
            
            saveResult <- saveGameSession newSession
            case saveResult of
                Left err -> return $ Left $ "Failed to create game session: " ++ err
                Right _  -> return $ Right newSession

createHaskemonForUser :: String -> String -> Int -> Int -> Int -> Int -> String -> IO ApiResponse
createHaskemonForUser username name hp mana atk def elementStr = do
    case parseElement elementStr of
        Nothing -> return ApiResponse
            { code = 400
            , success = False
            , message = "Invalid element type."
            , dataFields = Map.empty
            }
        Just element -> do
            case mkHaskemonStats atk def of
                Nothing -> return ApiResponse
                    { code = 400
                    , success = False
                    , message = "Invalid Haskemon stats: attack + defense exceeds 100."
                    , dataFields = Map.empty
                    }
                Just validStats -> do
                    let haskemon = HaskemonModel name hp mana element validStats username
                    result <- saveHaskemon haskemon
                    case result of
                        Left err -> return ApiResponse
                            { code = 500
                            , success = False
                            , message = "Failed to save Haskemon: " ++ err
                            , dataFields = Map.empty
                            }
                        Right objectId -> return ApiResponse
                            { code = 200
                            , success = True
                            , message = "Haskemon created successfully."
                            , dataFields = Map.fromList [("haskemonKey", toJSON $ show objectId)]
                            }

physicalAttack :: Int -> Int -> Int
physicalAttack attack defense = max (attack - defense) 0

magicAttack :: Int -> Element -> Element -> Int
magicAttack physicalAttackerDamage attackerElement targetElement
    | attackerElement > targetElement = floor (1.5 * fromIntegral physicalAttackerDamage)
    | targetElement < attackerElement = floor (0.7 * fromIntegral physicalAttackerDamage)
    | otherwise = physicalAttackerDamage

areaAttack :: Int -> Int -> Int
areaAttack magicAttackPower aliveEnemies = magicAttackPower `div` aliveEnemies

modifyStat :: Int -> HaskemonStats -> HaskemonStats
modifyStat delta stats = stats { attack = attack stats + delta, defense = defense stats + delta }

buff :: Bool -> Int -> HaskemonStats -> HaskemonStats
buff isTeam delta stats
    | isTeam = modifyStat delta stats { attack = attack stats + 7, defense = defense stats + 7 }
    | otherwise = modifyStat delta stats { attack = attack stats + 15, defense = defense stats + 15 }

debuff :: Bool -> Int -> HaskemonStats -> HaskemonStats
debuff isTeam delta stats
    | isTeam = modifyStat (-delta) stats { attack = attack stats - 7, defense = defense stats - 7 }
    | otherwise = modifyStat (-delta) stats { attack = attack stats - 15, defense = defense stats - 15 }

