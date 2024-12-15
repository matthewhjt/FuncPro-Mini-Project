{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Haskemon.Service.HaskemonService
    ( createHaskemonForUser, createGameSession, findHaskemonSession, playHaskemon
    ) where

import Web.Scotty ( json, pathParam, ActionM, jsonData, status )
import Haskemon.Model.HaskemonModel
import Haskemon.Model.HaskemonSessionModel
import Haskemon.Repository.HaskemonRepository (saveHaskemon, getHaskemonsByIds)
import Haskemon.Repository.HaskemonSessionRepository (createHaskemonSession, findHaskemonSessionById, updateHaskemonSession)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON)
import Data.Bson (ObjectId, Value)
import Data.Text (Text)
import Lib (ApiResponse(..), safeCreateObjectId, errorResponse, successResponse)
import qualified Data.Map as Map
import Data.Maybe
import System.Random (randomRIO)
import Network.HTTP.Types.Status (badRequest400, notFound404)

handleMoveOutcome :: Value -> [HaskemonModel] -> [HaskemonModel] -> Int -> Int -> ActionM ()
handleMoveOutcome sessionId team1 team2 newWinner currentHaskemon = do
    case (newWinner, currentHaskemon) of
        (0, 2) -> do
            let sysMoves = systemMoves team1 team2 sessionId
            saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
            let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
            json response
        (_, _) -> do
            let responseMsg = case newWinner of
                                1 -> "Game won"
                                2 -> "Game lost"
                                _ -> "Move saved successfully"
            let updatedGameSession = HaskemonSession
                    { sessionId = sessionId
                    , team1 = team1
                    , team2 = team2
                    , winner = newWinner
                    , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                    }
            saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
            let response = ApiResponse
                    { code = 200
                    , success = True
                    , message = responseMsg
                    , dataFields = Map.fromList [("gameSession", toJSON saveGameSession)]
                    }
            json response

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
            status notFound404
            json notFoundResponse

        Just (HaskemonSession sessionId team1 team2 winner currentHaskemon) -> do
            if winner /= 0
            then do
                status badRequest400
                json badRequest
            else do
                req <- jsonData :: ActionM PlayHaskemonRequest
                let (moveType, target) = move req
                    curHaskemon = team1 !! currentHaskemon
                    targetHaskemon = team2 !! target
                if healthPoint curHaskemon <= 0 || healthPoint targetHaskemon <= 0
                then json $ errorResponse 400 "Invalid move: target is dead" Map.empty
                else case moveType of
                    1 -> do
                        let updatedTargetHaskemon = applySinglePhysicalAttack curHaskemon targetHaskemon
                            updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                            newWinner = getWinner team1 updatedTeam2
                        handleMoveOutcome sessionId team1 updatedTeam2 newWinner currentHaskemon
                    2 -> do
                        if mana curHaskemon < 15
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                                updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                                updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                                newWinner = getWinner updatedTeam1 updatedTeam2
                            handleMoveOutcome sessionId updatedTeam1 updatedTeam2 newWinner currentHaskemon
                    3 -> do
                        if mana curHaskemon < 15
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                                updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                                updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                                newWinner = getWinner updatedTeam1 updatedTeam2
                            handleMoveOutcome sessionId updatedTeam1 updatedTeam2 newWinner currentHaskemon
                    4 -> do
                        if mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                                updatedTargetHaskemon = applySingleBuff targetHaskemon
                                updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam1 = updateTeam updateTeam1 target updatedTargetHaskemon
                                newWinner = getWinner updatedTeam1 team2
                            handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    5 -> do
                        if mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                                updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam1 = applyMultipleBuff updateTeam1
                                newWinner = getWinner updatedTeam1 team2
                            handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    6 -> do
                        if mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                                updatedTargetHaskemon = applySingleDebuff targetHaskemon
                                updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                                newWinner = getWinner updatedTeam1 updatedTeam2
                            handleMoveOutcome sessionId updatedTeam1 updatedTeam2 newWinner currentHaskemon
                    7 -> do
                        if mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move: not enough mana" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                                updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                                updatedTeam2 = applyMultipleDebuff team2
                                newWinner = getWinner updatedTeam1 updatedTeam2
                            handleMoveOutcome sessionId updatedTeam1 updatedTeam2 newWinner currentHaskemon
                    8 -> do
                        let updatedCurHaskemon = updateHp (+15) curHaskemon
                            updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            newWinner = getWinner updatedTeam1 team2
                        handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    9 -> do
                        let updatedTeam1 = updateTeamHp (+5) team1
                            newWinner = getWinner updatedTeam1 team2
                        handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    10 -> do
                        let updatedCurHaskemon = updateMana (+15) curHaskemon
                            updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            newWinner = getWinner updatedTeam1 team2
                        handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    11 -> do
                        let updatedTeam1 = updateTeamMana (+5) team1
                            newWinner = getWinner updatedTeam1 team2
                        handleMoveOutcome sessionId updatedTeam1 team2 newWinner currentHaskemon
                    _ -> do
                        status badRequest400
                        json badRequest

systemMoves :: [HaskemonModel] -> [HaskemonModel] -> Value -> [HaskemonSession]
systemMoves team enemy sessionId = do
    let fstTargetTeam = findFirstAliveHaskemon team 0
        fstAttEnemy = findFirstAliveHaskemon enemy 0
        updatedTargetHaskemon = applySinglePhysicalAttack (enemy !! fstAttEnemy) (team !! fstTargetTeam)
        updatedTeam = updateTeam team fstTargetTeam updatedTargetHaskemon
        newWinner = getWinner updatedTeam enemy
        sndAttEnemy = findFirstAliveHaskemon enemy (fstAttEnemy + 1)
        fstSysMove = HaskemonSession
                    { sessionId = sessionId
                    , team1 = updatedTeam
                    , team2 = enemy
                    , winner = newWinner
                    , currentHaskemon = findFirstAliveHaskemon updatedTeam 0
                    }

    case (newWinner, sndAttEnemy) of
        (2, _) -> [fstSysMove]
        (_, -1) -> [fstSysMove]
        (_, _) -> do
            let sndTargetTeam = findFirstAliveHaskemon updatedTeam 0
                updatedTargetHaskemon2 = applySinglePhysicalAttack (enemy !! sndAttEnemy) (team !! sndTargetTeam)
                updatedTeam2 = updateTeam updatedTeam sndTargetTeam updatedTargetHaskemon2
                newWinner2 = getWinner updatedTeam2 enemy
                thdAttEnemy = findFirstAliveHaskemon enemy (sndAttEnemy + 1)
                sndSysMove = HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam2
                            , team2 = enemy
                            , winner = newWinner2
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam2 0
                            }

            case (newWinner2, thdAttEnemy) of
                (2, _) -> [fstSysMove, sndSysMove]
                (_, -1) -> [fstSysMove, sndSysMove]
                (_, _) ->
                    let thdTargetTeam = findFirstAliveHaskemon updatedTeam2 0
                        updatedTargetHaskemon3 = applySinglePhysicalAttack (enemy !! thdAttEnemy) (team !! thdTargetTeam)
                        updatedTeam3 = updateTeam updatedTeam2 thdTargetTeam updatedTargetHaskemon3
                        newWinner3 = getWinner updatedTeam3 enemy
                        thdSysMove = HaskemonSession
                                    { sessionId = sessionId
                                    , team1 = updatedTeam3
                                    , team2 = enemy
                                    , winner = newWinner3
                                    , currentHaskemon = findFirstAliveHaskemon updatedTeam3 0
                                    }

                    in  [fstSysMove, sndSysMove, thdSysMove]

findFirstAliveHaskemon :: [HaskemonModel] -> Int -> Int
findFirstAliveHaskemon team startIdx
    | startIdx >= length team = -1
    | healthPoint (team !! startIdx) > 0 = startIdx
    | otherwise = findFirstAliveHaskemon team (startIdx + 1)

getWinner :: [HaskemonModel] -> [HaskemonModel] -> Int
getWinner team1 team2
    | all (\h -> healthPoint h <= 0) team1 = 2
    | all (\h -> healthPoint h <= 0) team2 = 1
    | otherwise = 0

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

    let stats = HaskemonStats { attack = 20, defense = 20 }
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

        response = successResponse 200 "Haskemon Session created." (Map.fromList [("gameSession", toJSON newSession)])
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
                        Left err -> return $ errorResponse 500 ("Failed to save Haskemon: " ++ err) Map.empty
                        Right objectId -> return $ successResponse 200 "Haskemon created successfully." (Map.fromList [("haskemonKey", toJSON $ show objectId)])

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
applyMultipleBuff = map applyAreaBuff
  where
    applyAreaBuff :: HaskemonModel -> HaskemonModel
    applyAreaBuff target = target {stats = areaBuff (stats target)}

applySingleDebuff :: HaskemonModel -> HaskemonModel
applySingleDebuff target = target {stats = singleDebuff (stats target)}

applyMultipleDebuff :: [HaskemonModel] -> [HaskemonModel]
applyMultipleDebuff = map applyAreaDebuff
  where
    applyAreaDebuff :: HaskemonModel -> HaskemonModel
    applyAreaDebuff target = target {stats = areaDebuff (stats target)}