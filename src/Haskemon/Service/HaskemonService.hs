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
import Data.Bson (ObjectId, Value)
import Data.Text (Text)
import Lib (ApiResponse(..), safeCreateObjectId, errorResponse, successResponse)
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
                        if healthPoint curHaskemon <= 0 || healthPoint targetHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedTargetHaskemon = applySinglePhysicalAttack curHaskemon targetHaskemon
                            let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                            let newWinner = getWinner team1 updatedTeam2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves team1 updatedTeam2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                        let responseMsg = 
                                                case newWinner of
                                                    1 -> "Game won"
                                                    2 -> "Game lost"
                                                    _ -> "Move saved successfully"
                                        let updatedGameSession = HaskemonSession
                                                { sessionId = sessionId
                                                , team1 = team1
                                                , team2 = updatedTeam2
                                                , winner = newWinner
                                                , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                                }
                                        saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                        let response = ApiResponse
                                                { code = 200
                                                , success = True
                                                , message = responseMsg
                                                , dataFields = Map.fromList
                                                    [
                                                        ("gameSession", toJSON saveGameSession)
                                                    ]
                                                }
                                        json response
                    2 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 15 || healthPoint targetHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                            let updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                            let newWinner = getWinner team1 updatedTeam2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 updatedTeam2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = updatedTeam2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    3 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 15 || healthPoint targetHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 15) curHaskemon
                            let updatedTargetHaskemon = applyMagicAttack curHaskemon targetHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                            let newWinner = getWinner team1 updatedTeam2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 updatedTeam2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = updatedTeam2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    4 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team1 !! target
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 5 || healthPoint targetHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                            let updatedTargetHaskemon = applySingleBuff targetHaskemon
                            let updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam1 = updateTeam updateTeam1 target updatedTargetHaskemon
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    5 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                            let updateTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam1 = applyMultipleBuff updateTeam1
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    6 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        let targetHaskemon = team2 !! target
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 5 || healthPoint targetHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                            let updatedTargetHaskemon = applySingleDebuff targetHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam2 = updateTeam team2 target updatedTargetHaskemon
                            let newWinner = getWinner updatedTeam1 updatedTeam2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 updatedTeam2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = updatedTeam2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    7 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0 || mana curHaskemon < 5
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (subtract 5) curHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let updatedTeam2 = applyMultipleDebuff team2
                            let newWinner = getWinner updatedTeam1 updatedTeam2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 updatedTeam2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = updatedTeam2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    8 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateHp (+15) curHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    9 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedTeam1 = updateTeamHp (+5) team1
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    10 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedCurHaskemon = updateMana (+15) curHaskemon
                            let updatedTeam1 = updateTeam team1 currentHaskemon updatedCurHaskemon
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    11 -> do
                        let curHaskemon = team1 !! currentHaskemon
                        if healthPoint curHaskemon <= 0
                        then json $ errorResponse 400 "Invalid move" Map.empty
                        else do
                            let updatedTeam1 = updateTeamMana (+5) team1
                            let newWinner = getWinner updatedTeam1 team2
                            case (newWinner, currentHaskemon) of
                                (0, 2) -> do
                                    let sysMoves = systemMoves updatedTeam1 team2 sessionId
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId $ last sysMoves
                                    let response = successResponse 200 "Move saved successfully" (Map.fromList [("systemMoves", toJSON sysMoves)])
                                    json response
                                (_, _) -> do
                                    let responseMsg = 
                                            case newWinner of
                                                1 -> "Game won"
                                                2 -> "Game lost"
                                                _ -> "Move saved successfully"
                                    let updatedGameSession = HaskemonSession
                                            { sessionId = sessionId
                                            , team1 = updatedTeam1
                                            , team2 = team2
                                            , winner = newWinner
                                            , currentHaskemon = (currentHaskemon + 1) `mod` length team1
                                            }
                                    saveGameSession <- liftIO $ updateHaskemonSession sessionId updatedGameSession
                                    let response = ApiResponse
                                            { code = 200
                                            , success = True
                                            , message = responseMsg
                                            , dataFields = Map.fromList
                                                [
                                                    ("gameSession", toJSON saveGameSession)
                                                ]
                                            }
                                    json response
                    _ -> json badRequest

systemMoves :: [HaskemonModel] -> [HaskemonModel] -> Value -> [HaskemonSession]
systemMoves team enemy sessionId = do
    let fstTargetTeam = findFirstAliveHaskemon team 0
        fstAttEnemy = findFirstAliveHaskemon enemy 0
        updatedTargetHaskemon = applySinglePhysicalAttack (enemy !! fstAttEnemy) (team !! fstTargetTeam)
        updatedTeam = updateTeam team fstTargetTeam updatedTargetHaskemon
        newWinner = getWinner updatedTeam enemy
        sndAttEnemy = findFirstAliveHaskemon enemy (fstAttEnemy + 1)

    case (newWinner, sndAttEnemy) of
        (2, _) -> [HaskemonSession
                    { sessionId = sessionId
                    , team1 = updatedTeam
                    , team2 = enemy
                    , winner = newWinner
                    , currentHaskemon = -1
                    }]
        (_, -1) -> [HaskemonSession
                    { sessionId = sessionId
                    , team1 = updatedTeam
                    , team2 = enemy
                    , winner = newWinner
                    , currentHaskemon = findFirstAliveHaskemon updatedTeam 0
                    }]
        (_, _) -> do
            let sndTargetTeam = findFirstAliveHaskemon updatedTeam 0
                updatedTargetHaskemon2 = applySinglePhysicalAttack (enemy !! sndAttEnemy) (team !! sndTargetTeam)
                updatedTeam2 = updateTeam updatedTeam sndTargetTeam updatedTargetHaskemon2
                newWinner2 = getWinner updatedTeam2 enemy
                thdAttEnemy = findFirstAliveHaskemon enemy (sndAttEnemy + 1)

            case (newWinner2, thdAttEnemy) of
                (2, _) -> [HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam
                            , team2 = enemy
                            , winner = newWinner
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam 0
                            }, 
                            HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam2
                            , team2 = enemy
                            , winner = newWinner2
                            , currentHaskemon = -1
                            }]
                (_, -1) -> [HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam
                            , team2 = enemy
                            , winner = newWinner
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam 0
                            }, 
                            HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam2
                            , team2 = enemy
                            , winner = newWinner2
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam2 0
                            }]
                (_, _) -> 
                    let thdTargetTeam = findFirstAliveHaskemon updatedTeam2 0
                        updatedTargetHaskemon3 = applySinglePhysicalAttack (enemy !! thdAttEnemy) (team !! thdTargetTeam)
                        updatedTeam3 = updateTeam updatedTeam2 thdTargetTeam updatedTargetHaskemon3
                        newWinner3 = getWinner updatedTeam3 enemy
                    
                    in  [ HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam
                            , team2 = enemy
                            , winner = newWinner
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam 0
                            }, 
                          HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam2
                            , team2 = enemy
                            , winner = newWinner2
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam2 0
                            },
                          HaskemonSession
                            { sessionId = sessionId
                            , team1 = updatedTeam3
                            , team2 = enemy
                            , winner = newWinner3
                            , currentHaskemon = findFirstAliveHaskemon updatedTeam3 0
                            }
                        ]



        

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