module Haskemon.Service.HaskemonService
    ( createHaskemonForUser, createGameSession, findHaskemonSession, playHaskemon
    ) where

import Web.Scotty ( json, pathParam, ActionM, jsonData )
import Haskemon.Model.HaskemonModel
import Haskemon.Model.HaskemonSessionModel
import Haskemon.Repository.HaskemonRepository (saveHaskemon, getHaskemonsByIds)
import Haskemon.Repository.HaskemonSessionRepository (createHaskemonSession, findHaskemonSessionById, updateHaskemonSession)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Bson (ObjectId)
import Data.Text (Text)
import Lib (ApiResponse(..), safeCreateObjectId)
import qualified Data.Map as Map
import Data.Maybe
import System.Random (randomRIO)

playHaskemon :: ActionM ()
playHaskemon = do
    haskemonSessionId <- pathParam "haskemonSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId haskemonSessionId
    gameSession <- liftIO $ findHaskemonSessionById gameSessionObjectId

    let notFoundResponse = ApiResponse
                        { code = 404
                        , success = False
                        , message = "GameSession Not Found"
                        , dataFields = Map.empty
                        }

    let badRequest = ApiResponse
                        { code = 400
                        , success = False
                        , message = "Game Already won."
                        , dataFields = Map.empty
                        }

    case gameSession of
        Nothing -> do
            json notFoundResponse

        Just (HaskemonSession sessionId team1 team2 winner currentHaskemon) -> do
            if winner /= 0
            then do
                json badRequest
            else do
                req <- jsonData :: ActionM PlayHaskemonRequest
                let (moveType, target) = move req
                case moveType of
                    1 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        let updatedTargetHaskemon = applySinglePhysicalAttack curHaskemon targetHaskemon
                        let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = team1
                                , team2 = updatedTeam2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    2 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                        let updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = updatedTeam2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    3 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                        let updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = updatedTeam2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    4 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team1 !! target
                        let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                        let updatedTargetHaskemon = applySingleBuff targetHaskemon
                        let updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam1 = updateTeam updateTeam1 target updatedTargetHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    5 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                        let updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam1 = applyMultipleBuff team1
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    6 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                        let updatedTargetHaskemon = applySingleDebuff targetHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = updatedTeam2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    7 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedTeam2 = applyMultipleDebuff team2
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = updatedTeam2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    8 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let updatedCurHaskemon = updateHp (+15) curHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    9 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let updatedTeam1 = updateTeamHp (+5) team1
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    10 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let updatedCurHaskemon = updateMana (+15) curHaskemon
                        let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    11 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let updatedTeam1 = updateTeamMana (+5) team1
                        let updatedGameSession = HaskemonSession
                                { sessionId = sessionId
                                , team1 = updatedTeam1
                                , team2 = team2
                                , winner = winner
                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                }
                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                        let response = ApiResponse
                                { code = 200
                                , success = True
                                , message = "Move saved successfully."
                                , dataFields = Map.fromList
                                    [
                                        ("gameSession", toJSON saveGameSession)
                                    ]
                                }
                        json response
                    _ -> json badRequest

updateTeam :: [HaskemonModel] -> Int -> HaskemonModel -> [HaskemonModel]
updateTeam team idx updatedHaskemon =
    let (before, _ : after) = splitAt idx team
    in before ++ [updatedHaskemon] ++ after

randomElement :: IO Element
randomElement = do
    randomIndex <- randomRIO (0, 3) :: IO Int
    return $ case randomIndex of
        0 -> Fire
        1 -> Water
        2 -> Earth
        3 -> Air

createGameSession :: String -> [ObjectId] -> IO ApiResponse
createGameSession username teamIds  = do
    team1Haskemons <- getHaskemonsByIds teamIds

    let stats = HaskemonStats { attack = 50, defense = 50 }
    elem1 <- randomElement
    elem2 <- randomElement
    elem3 <- randomElement

    let haskemon1 = HaskemonModel { name = "Bandit1", healthPoint = 100, mana = 100, element = elem1, stats = stats, ownerUsername = "adm1n"}
    let haskemon2 = HaskemonModel { name = "Bandit2", healthPoint = 100, mana = 100, element = elem2, stats = stats, ownerUsername = "adm1n"}
    let haskemon3 = HaskemonModel { name = "Bandit3", healthPoint = 100, mana = 100, element = elem3, stats = stats, ownerUsername = "adm1n"}

    let team2Haskemons = [haskemon1, haskemon2, haskemon3]
     
    gameSessionId <- liftIO $ createHaskemonSession team1Haskemons team2Haskemons
    let newSession = HaskemonSession
            { sessionId = gameSessionId 
            , team1 = team1Haskemons
            , team2 = team2Haskemons
            , winner = 0
            , currentHaskemon = 0
            }

    let response = ApiResponse
            { code = 200
            , success = True
            , message = "Move saved successfully."
            , dataFields = Map.fromList
                [
                    ("gameSession", toJSON newSession)
                ]
            }
    return response

findHaskemonSession :: ActionM()
findHaskemonSession = do
    haskemonSessionId <- pathParam "haskemonSessionId" :: ActionM String
    let gameSessionObjectId = safeCreateObjectId haskemonSessionId
    gameSession <- liftIO $ findHaskemonSessionById gameSessionObjectId

    let notFoundResponse = ApiResponse
                        { code = 404
                        , success = False
                        , message = "HaskemonSession Not Found"
                        , dataFields = Map.empty
                        }

    case gameSession of
        Just gS -> json gS
        Nothing -> json notFoundResponse

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

singleBuff :: HaskemonStats -> HaskemonStats
singleBuff = modifyStat 15

areaBuff :: HaskemonStats -> HaskemonStats
areaBuff = modifyStat 6

singleDebuff :: HaskemonStats -> HaskemonStats
singleDebuff = modifyStat (-15)

areaDebuff :: HaskemonStats -> HaskemonStats
areaDebuff = modifyStat (-6)

applySinglePhysicalAttack :: HaskemonModel -> HaskemonModel -> HaskemonModel
applySinglePhysicalAttack attacker target = target { healthPoint = healthPoint target - physicalAttack (attack (stats attacker)) (defense (stats target)) }

applyMagicAttack :: HaskemonModel -> HaskemonModel -> HaskemonModel
applyMagicAttack attacker target = target { healthPoint = healthPoint target - magicAttack (attack (stats attacker)) (element attacker) (element target) }

applyAreaMagicAttack :: HaskemonModel -> [HaskemonModel] -> [HaskemonModel]
applyAreaMagicAttack attacker team = map (applyAreaAttack attacker) team
  where
    applyAreaAttack :: HaskemonModel -> HaskemonModel -> HaskemonModel
    applyAreaAttack attacker target = target { healthPoint = healthPoint target - areaAttack (attack (stats attacker)) (length team) }

applySingleBuff :: HaskemonModel -> HaskemonModel
applySingleBuff target = target {stats = singleBuff (stats target)} 

applyMultipleBuff :: [HaskemonModel] -> [HaskemonModel]
applyMultipleBuff team = map applyAreaBuff team
  where
    applyAreaBuff :: HaskemonModel -> HaskemonModel
    applyAreaBuff target = target {stats = areaBuff (stats target)}

applySingleDebuff :: HaskemonModel -> HaskemonModel
applySingleDebuff target = target {stats = singleDebuff (stats target)} 

applyMultipleDebuff :: [HaskemonModel] -> [HaskemonModel]
applyMultipleDebuff team = map applyAreaDebuff team
  where
    applyAreaDebuff :: HaskemonModel -> HaskemonModel
    applyAreaDebuff target = target {stats = areaDebuff (stats target)}