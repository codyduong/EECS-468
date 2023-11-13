-- Author: Cody Duong
-- Creation Date: 13 November 2023
-- Other Sources:
--    * https://wiki.haskell.org/List_comprehension
--    * https://www.haskell.org/tutorial/modules.html
--    * https://www.haskell.org/tutorial/io.html
--
-- Main file for Assignment7 code

module Main(main) where

import Replicate'(replicate')
import Perfects(perfects)
import Find(find)
import Positions'(positions')
import Scalarproduct(scalarproduct)

main :: IO ()
main = do
  putStrLn "> replicate' 3 True"
  print $ show (replicate' 3 True)
  putStrLn "> replicate' 5 \"test code\""
  print $ show (replicate' 5 "test code")

  putStrLn "> perfects 500"
  print $ show (perfects 500)
  putStrLn "> perfects 9000"
  print $ show (perfects 9000)

  putStrLn "> find 'b' [('a',1),('b',2),('c',3),('b',4)]"
  print $ show (find 'b' [('a',1),('b',2),('c',3),('b',4)])
  putStrLn "> find 'c' [('a',1),('b',2),('c',3),('b',4),('c',25)]"
  print $ show (find 'c' [('a',1),('b',2),('c',3),('b',4),('c',25)])

  putStrLn "> positions' 0 [1,0,0,1,0,1,1,0]"
  print $ show (positions' 0 [1,0,0,1,0,1,1,0])
  putStrLn "> positions' 1 [1,0,0,1,0,1,1,0]"
  print $ show (positions' 1 [1,0,0,1,0,1,1,0])

  putStrLn "> scalarproduct [1,2,3] [4,5,6]"
  print $ show (scalarproduct [1,2,3] [4,5,6])
  putStrLn "> scalarproduct [-1,2,3] [-4,-5,6]"
  print $ show (scalarproduct [-1,2,3] [-4,-5,6])