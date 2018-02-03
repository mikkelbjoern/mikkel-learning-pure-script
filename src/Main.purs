module Main where

import Control.Monad.Eff.Console

import Prelude (mod, (+), (-), (==))
import Data.HeytingAlgebra((||))


f:: Int -> Int -> Int
f x 0 = x
f x y =
  if ((y `mod` 3) == 0) || ((y `mod` 5) == 0)
    then f (x+y) (y-1)
    else f x (y-1)


main = logShow (f 0 999)
