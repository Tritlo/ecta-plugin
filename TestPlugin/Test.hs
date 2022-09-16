{-# OPTIONS_GHC -fplugin ECTA.Plugin
                -fno-max-valid-hole-fits
                -fplugin-opt=ECTA.Plugin:expr-size=5 #-}
module Test (main) where

import Prelude (Bool(..), putStrLn, undefined, Eq((==)), Int, reverse, Maybe(..))
import Data.Maybe (mapMaybe)


equal :: Eq a => [a] -> [a] -> Bool
equal = _


myMapMaybe :: (g -> Maybe b) -> [g] -> [b]
myMapMaybe f xs = _

-- prop_reverse :: [Int] -> Bool
-- prop_reverse xs = _
-- prop_reverse xs = xs == reverse (reverse xs)



main = putStrLn "hello, ecta"
