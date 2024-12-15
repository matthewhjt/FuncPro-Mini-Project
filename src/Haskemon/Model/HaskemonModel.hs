module Haskemon.Model.HaskemonModel
    ( HaskemonModel (..)
    , Element (..)
    , HaskemonStats(..)
    , mkHaskemonStats
    )
    where

data HaskemonModel = Haskemon
    { healthPoint :: HP,
      mana :: Mana,
      element :: Element,
      stats :: HaskemonStats
    } deriving (Show)

type HP = Int
type Mana = Int

data Element = Fire | Water | Earth | Air
    deriving (Show, Eq)

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

data HaskemonStats = HaskemonStats
    { attack :: Int
    , defense :: Int
    } deriving (Show)

mkHaskemonStats :: Int -> Int -> Maybe HaskemonStats
mkHaskemonStats atk def
    | atk + def <= 100 = Just (HaskemonStats atk def)
    | otherwise = Nothing

updateHp :: (HP -> HP) -> HaskemonModel -> HaskemonModel
updateHp f haskemon = haskemon { healthPoint = f (healthPoint haskemon) }

updateMana :: (Mana -> Mana) -> HaskemonModel -> HaskemonModel
updateMana f haskemon = haskemon { mana = f (mana haskemon)}

updateTeamHp :: (HP -> HP) -> [HaskemonModel] -> [HaskemonModel]
updateTeamHp f = map (updateHp f)

updateTeamMana :: (Mana -> Mana) -> [HaskemonModel] -> [HaskemonModel]
updateTeamMana f = map (updateMana f)