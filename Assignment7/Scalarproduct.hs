-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
--
-- Haskell Function for calculating scalar products from two vectors

module Scalarproduct(scalarproduct) where

scalarproduct :: [Int] -> [Int] -> Int
-- The function takes two lists, xs and ys, as arguments
-- zips together xs and ys to create every required pair and multiplies each pair, and then summations it
-- to return a scalarproduct
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]