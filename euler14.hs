module Euler1 where

import Data.List
import Data.Char

next n  | even n = n `div` 2
        | otherwise = 3*n + 1
        

getList n   | n == 1 = [1]
            | otherwise = n : getList (next n)
            
len n = length (getList n)


triangle n = sum[1..n]

l n   | n == 0 = []
      | otherwise =  ++ [1]

