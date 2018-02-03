module Main where

import Control.Monad.Eff.Console

import Data.HeytingAlgebra ((||))
import Prelude (mod, (+), (-), (==), (>))


f:: Int -> Int -> Int
f x 0 = x
f x y =
  if ((y `mod` 3) == 0) || ((y `mod` 5) == 0)
    then f (x+y) (y-1)
    else f x (y-1)

fib:: Int -> Int
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)

even:: Int -> Boolean
even x =
  if x `mod` 2 == 0
    then true
    else false

euler2:: Int -> Int
euler2 n =
  if fib n > 4000000
    then 0
    else
      if even (fib n)
        then euler2 (n+1) + fib n
        else euler2 (n+1)

main = logShow (euler2 1)
