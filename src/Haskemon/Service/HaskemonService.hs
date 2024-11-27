module Haskemon.Service.HaskemonService where

import Haskemon.Model.HaskemonModel (Element(..), HaskemonStats(..))

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

