-- Author: Cody Duong
-- Creation Date: 26 November 2023
-- Other Sources:
--    In no particular order:
--    * https://www.haskell.org/tutorial/modules.html
--    * https://www.haskell.org/onlinereport/derived.html#derived-appendix
--    * https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Either.html
--    * https://hoogle.haskell.org/?hoogle=span
--    * https://hoogle.haskell.org/?hoogle=elem
--        * https://stackoverflow.com/a/32166941/17954209
--    * https://wiki.haskell.org/Regular_expressions
--
--    * This is a rehash of EECS348 project, but instead of cpp its Haskell!
--        * https://github.com/codyduong/EECS-348-Project/tree/main/src
--
--    * https://www.cs.toronto.edu/~lczhang/324/lab/lab09.pdf
--        * Typeclasses are not a concept I'm super familiar with, it reads like function implementation overloading dependent on specific arguments
--        * https://en.wikipedia.org/wiki/Ad_hoc_polymorphism#Example
--
--    * http://zvon.org/other/haskell/Outputprelude/filter_f.html
--    * https://hoogle.haskell.org/?hoogle=isSpace
--
--    * https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Applicative.html
--        * wow. what a cool feature.
--
-- Haskell Parser with operator precedence by reading left to right, and parsing "deepest" (ie. highest precedence) operators first
-- It reads "inside out" when following stack trace

module Parse(parse) where

import Data.Char (isDigit, isSpace)

-- Data type to represent the arithmetic expressions
data Expr = 
          -- Binary Ops
          Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Pow Expr Expr
          -- Unary Ops
          | Neg Expr
          | Const Double

          -- tbh deriving is very confusing to me, similiar to generics in other languages?
          -- implements a "Show" for I/O and "Eq" for equality checks
          deriving (Show, Eq)

removeSpace :: String -> String
removeSpace = filter (not . isSpace)

-- Parse and evaluate an arithmetic expression
parse :: String -> IO ()
parse input =
  -- As a quick aside, this is nested way too deep, but too late!

  -- Remove all whitespace (it breaks the parser, because whitespace is expected to be the end, and besides expressions aren't space delimited so who cares)
  case parseExpr (removeSpace input) of
    Left err -> putStrLn $ "Error: " ++ err
    Right (expr, rest) ->
      if all isSpace rest
        then case evaluate expr of
          Left err -> putStrLn $ "Error: " ++ err
          Right dbl -> print dbl
        -- Check some fail cases
        else
          -- Check if we accidentally ingested a unsupported expression
          case parseUnsupported rest of
            Left err -> putStrLn $ "Error: " ++ err
            Right (expr, rest) ->
              if all (== ')') rest
                then putStrLn "Error: Mismatched parentheses."
                else putStrLn "Error: Failed to parse the input."

-- Validate expression (chunked), this varies against checkValidCharacter in that it checks whole "chunks" of
-- expressions, and it can be clunkier as a consequence
parseUnsupported :: String -> Either String (Expr, String)
parseUnsupported input@(c:_)
  | c `notElem` supportedOperators && not (isNumber [c]) && c `notElem` "()" =
    Left $ "Expected operator, received: " ++ [c]
  | otherwise =
    Left $ "Expected operator, received: " ++ input
  where
    supportedOperators = ['+', '-', '*', '/', '%']
    isNumber str = case reads str :: [(Double, String)] of
      [(num, "")] -> True
      _ -> False

-- Parse an arithmetic expression
parseExpr :: String -> Either String (Expr, String)
parseExpr input =
  -- This is terrible, but it works...
  case checkValidCharacters input of
    Left err -> Left err
    Right () ->
      case parseAddSub input of
        Left err -> Left err
        Right (expr, rest) -> parseAddSubRest expr rest

-- Check valid characters throughout whole expression
checkValidCharacters :: String -> Either String ()
checkValidCharacters input =
  let invalidChars = filter (not . isValidChar) input
   in if null invalidChars
        then Right ()
        else Left $ "Invalid characters: " ++ invalidChars

-- this is unfortunately not regex
isValidChar :: Char -> Bool
isValidChar c = c `elem` "0123456789()+-*/%"

-- Parse addition and subtraction expressions
parseAddSub :: String -> Either String (Expr, String)
parseAddSub input =
  case parseMulDivMod input of
    Left err -> Left err
    Right (expr, rest) -> parseAddSubRest expr rest

-- Parse the rest of addition and subtraction expressions
parseAddSubRest :: Expr -> String -> Either String (Expr, String)
parseAddSubRest expr (c:rest) | c `elem` "+-" =
  case parseMulDivMod rest of
    Left _ -> Left "Failed to parse rest of addition/subtraction."
    Right (expr', rest') ->
      parseAddSubRest (if c == '+' then Add expr expr' else Sub expr expr') rest'
parseAddSubRest expr rest = Right (expr, rest)

-- Parse multiplication, division, and modulo expressions
parseMulDivMod :: String -> Either String (Expr, String)
parseMulDivMod input =
  case parsePow input of
    Left err -> Left err
    Right (expr, rest) -> parseMulDivModRest expr rest

-- Parse the rest of multiplication, division, and modulo expressions
parseMulDivModRest :: Expr -> String -> Either String (Expr, String)
parseMulDivModRest expr (c:rest) | c `elem` "*/%" =
  case parsePow rest of
    Left _ -> Left "Failed to parse rest of multiplication/division/modulo."
    Right (expr', rest') ->
      parseMulDivModRest (case c of '*' -> Mul expr expr'; '/' -> Div expr expr'; '%' -> Mod expr expr') rest'
parseMulDivModRest expr rest = Right (expr, rest)

-- Parse exponentiation expressions
parsePow :: String -> Either String (Expr, String)
parsePow input =
  case parseUnary input of
    Left err -> Left err
    Right (expr, rest) -> parsePowRest expr rest

-- Parse the rest of exponentiation expressions
parsePowRest :: Expr -> String -> Either String (Expr, String)
parsePowRest expr ('*':'*':rest) =
  case parseUnary rest of
    Left _ -> Left "Failed to parse rest of exponentiation."
    Right (expr', rest') -> Right (Pow expr expr', rest')
parsePowRest expr rest = Right (expr, rest)

-- Parse unary expressions (negation)
parseUnary :: String -> Either String (Expr, String)
parseUnary ('-':rest) =
  case parseAtom rest of
    Left err -> Left err
    Right (expr, rest') -> Right (Neg expr, rest')
parseUnary input = parseAtom input

-- Parse atomic expressions (constants or parenthesized expressions)
parseAtom :: String -> Either String (Expr, String)
parseAtom ('(':rest) =
  case parseExpr rest of
    Left err -> Left err
    Right (expr, ')':rest') -> Right (expr, rest')
    -- this has the unique case of only failing when we have more opening parentheses "(" than closing
    -- if there are more closing ")" parentheses this won't be caught, it'll just fail to parse at top level "parse"

    -- This error may not propogate as expected: 2 * (4 + 3 - 1
    -- Multiplication error takes precedence since it's LTR, the left error always takes precedence, rather than precedent errors... w/e
    -- Likewise, as expected (2 * (4 + 3 - 1) does throw this error correctly...
    _ -> Left "Mismatched opening parentheses."
parseAtom input =
  case parseNumber input of
    Left err -> Left err
    Right (num, rest) -> Right (Const num, rest)

-- Parse a number (integer or floating-point)
parseNumber :: String -> Either String (Double, String)
parseNumber input =
  case span (\c -> isDigit c || c == '.') input of
    ("", _) -> Left "Failed to parse a number."
    (numStr, rest) ->
      case reads numStr of
        [(num, "")] -> Right (num, rest)
        _ -> Left "Failed to parse a number."

-- Evaluate an arithmetic expression
evaluate :: Expr -> Either String Double
-- Applicatives are super strong? But also kind of confusing...
evaluate (Add e1 e2) = (+) <$> evaluate e1 <*> evaluate e2
evaluate (Sub e1 e2) = (-) <$> evaluate e1 <*> evaluate e2
evaluate (Mul e1 e2) = (*) <$> evaluate e1 <*> evaluate e2
evaluate (Div e1 e2) = do
  v1 <- evaluate e1
  v2 <- evaluate e2
  if v2 /= 0
    then Right (v1 / v2)
    else Left "DivideByZero"
evaluate (Mod e1 e2) = mod' <$> evaluate e1 <*> evaluate e2
-- https://stackoverflow.com/questions/14027865/modulus-remainder-function-for-non-integers
-- Something something, floating point precision sucks. I did not write this, but it handles doubles better than default mod
  where
    mod' x y = fromIntegral $ mod (round x :: Integer) (round y :: Integer)
evaluate (Pow e1 e2) = (**) <$> evaluate e1 <*> evaluate e2
evaluate (Neg e) = negate <$> evaluate e
evaluate (Const x) = Right x
