module Main where

import Control.Monad.Eff.Console(logShow)

-- import Data.HeytingAlgebra ((||))
import Math (sqrt)
import Prelude(map, mod, (&&), (+), (-), (==), (||), (/))
import Data.Array(foldl, (..), filter)
import Data.Int as Int
import Data.Eq
import Data.Ord(min, max)



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

checkList:: Int -> Array Int
checkList n = 2 .. Int.ceil (sqrt (Int.toNumber n))

isPrime:: Int -> Boolean
isPrime 2 = true
isPrime p =
  foldl (&&) true (map (\n -> p `mod` n /= 0) (checkList p))

minPrimeDiv:: Int -> Int
minPrimeDiv x =
  foldl min x (filter (\n -> (x `mod` n == 0 && isPrime n)) (checkList x))

biggestPrimeDiv:: Int -> Int
biggestPrimeDiv x =
  foldl max 0 (filter (\n -> (x `mod` n == 0 && isPrime n)) (1 .. x))



-- main:: forall t. Eff ( console :: CONSOLE | t ) Unit
main = logShow (biggestPrimeDiv 600851475143)
