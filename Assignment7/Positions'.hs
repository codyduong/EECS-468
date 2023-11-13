-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
--
-- Haskell Function for finding the positions where a value appears in a list

module Positions'(positions') where
import Find(find)

positions' :: Eq a => a -> [a] -> [Int]
-- Search for value x (type a), in [a] and return indices [Int]
-- Zip every value into the list into an arbitrary table, where the 2nd value is
-- the index (see [0..1]) and then find all values using the find functionality
-- we defined earlier, thus resulting in the indices [Int]
positions' x xs = find x (zip xs [0..])
