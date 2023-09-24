{-# OPTIONS_GHC -fplugin ECTA.Plugin
                -fno-max-valid-hole-fits
                -fplugin-opt=ECTA.Plugin:expr-size=5 #-}
module Test (main) where

import Data.Maybe (mapMaybe)


equal :: Eq a => [a] -> [a] -> Bool
equal = _


myMapMaybe :: (g -> Maybe b) -> [a] -> [b]
myMapMaybe f xs = _


main = putStrLn "hello, ecta"
