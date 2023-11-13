-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
-- 
-- Haskell Function for replicate using list comprehension

module Replicate'(replicate') where

replicate' :: Int -> a -> [a]
-- Takes in argument n integer, and generic type a with argument x to produce a list of that a type replicated n amount of times
-- IE. Replicate n amount of times with x "phrase"
replicate' n x = [x | _ <- [1..n]]