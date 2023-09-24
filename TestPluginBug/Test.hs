{-# OPTIONS_GHC -fplugin ECTA.Plugin
                -fno-max-valid-hole-fits
                -fplugin-opt=ECTA.Plugin:expr-size=5 #-}
module Test (main) where

-- We need the three of these, if we remove any it does not happen
import Prelude (pred, print, Monad(..), IO(..))

-- This can be Bool -> Bool also or possibly more,
-- but we remove Bool from the imports.
f :: a -> a
f = _

main :: IO ()
main = return ()

