-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
--    * https://oeis.org/A000396
--
-- Haskell Function for determing "Perfect" integers

module Perfects(perfects) where

-- this is horrible, see https://codereview.stackexchange.com/a/86706
-- but its kind of a moot point if i just copied this code, so i won't

-- also just seperate this out for sake of readability, noone likes reading nested list comprehensions in any language
factors :: Int -> [Int]
-- Brute force the factors of some integer x with guard clause where there is no remainder
factors n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
-- Find all perfect numbers under x integer and returns the list of perfect numbers [Int]
-- Brute force check all numbers under some argument x if it matches the guard clause where x = sum of factors(x)
perfects limit = [x | x <- [2..limit], x == sum (factors x)]