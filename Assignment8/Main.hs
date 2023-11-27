-- Author: Cody Duong
-- Creation Date: 26 November 2023
-- Other Sources:
--    * N/A
--
-- Main file for Assignment8 code

module Main(main) where

import Parse(parse)

main :: IO ()
main = do
  -- 1
  putStrLn "3+4"
  parse "3+4"
  -- 2
  putStrLn "8-(5-2)"
  parse "8-(5-2)"
  -- 3
  putStrLn "10 * 2 / 5"
  parse "10 * 2 / 5"
  -- 4
  putStrLn "2 ** 3"
  parse "2 ** 3"
  -- 5
  putStrLn "4 * (3 + 2) % 7 - 1"
  parse "4 * (3 + 2) % 7 - 1"
  -- E1
  putStrLn "2 * (4 + 3 - 1"
  parse "2 * (4 + 3 - 1"
  -- E2
  putStrLn "* 5 + 2"
  parse "* 5 + 2"
  -- E3
  putStrLn "4 / 0"
  parse "4 / 0"
  -- E4
  putStrLn "5(2+3)"
  parse "5(2+3)"
  -- E5
  putStrLn "7 & 3"
  parse "7 & 3"
  -- E6
  putStrLn "(((3 + 4) - 2) + (1)"
  parse "(((3 + 4) - 2) + (1)"
  -- E7
  putStrLn "((5 + 2) / (3 * 0))"
  parse "((5 + 2) / (3 * 0))"
  -- E8
  putStrLn "((2 -) 1 + 3)"
  parse "((2 -) 1 + 3)"
  -- E9
  putStrLn "((4 * 2) + ( - ))"
  parse "((4 * 2) + ( - ))"
  -- E10
  putStrLn "((7 * 3) ^ 2)"
  parse "((7 * 3) ^ 2)"