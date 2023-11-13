-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
--
-- Haskell Function for finding the values matching some lookup value

module Find(find) where

find :: Eq a => a -> [(a, b)] -> [b]
-- Check for argument key (type a) and return all values from table (type b) if found
-- using list conditional and guard clause
find key table = [value | (k, value) <- table, k == key]