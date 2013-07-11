module Main where

-- {BEGIN: intro}
--------------------------------------------------------------------------------
-- Basic data type with pattern matching.

--------------------------------------------------------------------------------
data IntWrapper = IntWrapper Int

--------------------------------------------------------------------------------
wrapInt :: Int -> IntWrapper
wrapInt x = IntWrapper x

--------------------------------------------------------------------------------
unwrapInt :: IntWrapper -> Int
unwrapInt (IntWrapper x) = x
-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = print (unwrapInt $ wrapInt 10)
