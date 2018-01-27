module Main where

import Control.Monad.Eff.Console
import Math (sqrt)
import Prelude

diagonal w h = sqrt (w * w  + h * h)

main = logShow (diagonal 4.0 3.0)
