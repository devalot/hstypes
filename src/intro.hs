module Main where

-- {BEGIN: intro}

--------------------------------------------------------------------------------
-- | Basic type using the same name for the type and data
-- constructors.  This data constructor is a function that takes an
-- Int and returns an IntWrapper.
data IntWrapper = IntWrapper Int

--------------------------------------------------------------------------------
-- | Using the data constructor.
wrapInt :: Int -> IntWrapper
wrapInt x = IntWrapper x

--------------------------------------------------------------------------------
-- | Patter matching with the data constructor.
unwrapInt :: IntWrapper -> Int
unwrapInt (IntWrapper x) = x

-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = print (unwrapInt $ wrapInt 10)
