{-# LANGUAGE DeriveGeneric #-}

module Haskemon.Model.HaskemonModel
    ( HaskemonModel (..)
    , Element (..)
    , HaskemonStats(..)
    , mkHaskemonStats
    , parseElement
    , updateMana
    , updateHp
    , updateTeamHp
    , updateTeamMana
    ) where

import GHC.Generics (Generic)
import Database.MongoDB (ObjectId)
import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), object, (.=), withObject, (.:), genericToJSON, genericParseJSON, defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bson

data HaskemonModel = HaskemonModel
    { name :: String
    , healthPoint :: HP
    , mana :: Mana
    , element :: Element
    , stats :: HaskemonStats
    , ownerUsername :: String
    } deriving (Show, Eq, Generic)

type HP = Int
type Mana = Int

data Element = Fire | Water | Earth | Air
    deriving (Show, Eq, Generic)

instance Ord Element where
    compare Fire Earth = GT
    compare Earth Fire = LT
    compare Earth Air  = GT
    compare Air Earth  = LT
    compare Air Water  = GT
    compare Water Air  = LT
    compare Water Fire = GT
    compare Fire Water = LT
    compare _ _        = EQ

instance ToJSON Element where
    toJSON Fire  = "Fire"
    toJSON Water = "Water"
    toJSON Earth = "Earth"
    toJSON Air   = "Air"

instance FromJSON Element where
    parseJSON "Fire"  = return Fire
    parseJSON "Water" = return Water
    parseJSON "Earth" = return Earth
    parseJSON "Air"   = return Air
    parseJSON _       = fail "Unknown Element"

instance Val Element where
    val Fire = String "Fire"
    val Water = String "Water"
    val Earth = String "Earth"
    val Air = String "Air"

    cast' (String "Fire") = Just Fire
    cast' (String "Water") = Just Water
    cast' (String "Earth") = Just Earth
    cast' (String "Air") = Just Air
    cast' _ = Nothing

data HaskemonStats = HaskemonStats
    { attack :: Int
    , defense :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON HaskemonStats where
    toJSON (HaskemonStats attack defense) = object
        [ "attack" .= attack
        , "defense" .= defense
        ]

instance FromJSON HaskemonStats where
    parseJSON = withObject "HaskemonStats" $ \v -> do
        attack <- v .: "attack"
        defense <- v .: "defense"
        return $ HaskemonStats attack defense

instance Val HaskemonStats where
    val stats = Doc 
        [ "attack" =: attack stats
        , "defense" =: defense stats
        ]

    cast' (Doc doc) = do
        atk <- Data.Bson.lookup "attack" doc
        def <- Data.Bson.lookup "defense" doc
        Just $ HaskemonStats atk def
    cast' _ = Nothing

instance ToJSON HaskemonModel where
    toJSON (HaskemonModel name healthPoint mana element stats ownerUsername) = object
        [ "name" .= name
        , "healthPoint" .= healthPoint
        , "mana" .= mana
        , "element" .= element
        , "stats" .= stats
        , "ownerUsername" .= ownerUsername
        ]

instance FromJSON HaskemonModel where
    parseJSON = withObject "HaskemonModel" $ \v -> do
        name <- v .: "name"
        healthPoint <- v .: "healthPoint"
        mana <- v .: "mana"
        element <- v .: "element"
        stats <- v .: "stats"
        ownerUsername <- v .: "ownerUsername"
        return $ HaskemonModel name healthPoint mana element stats ownerUsername

instance Val HaskemonModel where
    val haskemon = Doc
        [ "healthPoint" =: healthPoint haskemon
        , "mana" =: mana haskemon
        , "element" =: val (element haskemon)
        , "stats" =: val (stats haskemon)
        ]

    cast' (Doc doc) = do
        name <- Data.Bson.lookup "name" doc
        hp <- Data.Bson.lookup "healthPoint" doc
        mn <- Data.Bson.lookup "mana" doc
        elem <- Data.Bson.lookup "element" doc 
        sts <- Data.Bson.lookup "stats" doc
        owner <- Data.Bson.lookup "ownerUsername" doc
        Just $ HaskemonModel name hp mn elem sts owner
    cast' _ = Nothing

parseElement :: String -> Maybe Element
parseElement "Fire"  = Just Fire
parseElement "Water" = Just Water
parseElement "Earth" = Just Earth
parseElement "Air"   = Just Air
parseElement _       = Nothing

mkHaskemonStats :: Int -> Int -> Maybe HaskemonStats
mkHaskemonStats atk def
    | atk + def <= 100 = Just (HaskemonStats atk def)
    | otherwise = Nothing

updateHp :: (HP -> HP) -> HaskemonModel -> HaskemonModel
updateHp f haskemon = haskemon { healthPoint = f (healthPoint haskemon) }

updateMana :: (Mana -> Mana) -> HaskemonModel -> HaskemonModel
updateMana f haskemon = haskemon { mana = f (mana haskemon) }

updateTeamHp :: (HP -> HP) -> [HaskemonModel] -> [HaskemonModel]
updateTeamHp f = map (updateHp f)

updateTeamMana :: (Mana -> Mana) -> [HaskemonModel] -> [HaskemonModel]
updateTeamMana f = map (updateMana f)

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

applyMultipleBuff :: HaskemonModel -> [HaskemonModel] -> [HaskemonModel]
applyMultipleBuff performer team = map applyAreaBuff team
  where
    applyAreaBuff :: HaskemonModel -> HaskemonModel
    applyAreaBuff target = target {stats = areaBuff (stats target)}

applySingleDebuff :: HaskemonModel -> HaskemonModel
applySingleDebuff target = target {stats = singleDebuff (stats target)} 

applyMultipleDebuff :: HaskemonModel -> [HaskemonModel] -> [HaskemonModel]
applyMultipleDebuff performer team = map applyAreaDebuff team
  where
    applyAreaDebuff :: HaskemonModel -> HaskemonModel
    applyAreaDebuff target = target {stats = areaDebuff (stats target)}