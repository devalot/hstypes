module Main where

-- {BEGIN: rec}

--------------------------------------------------------------------------------
-- | Using record syntax to create a 'User' type.
data User = User
  { firstName    :: String
  , lastName     :: String
  , shoeSize     :: Float
  , likesHaskell :: Bool
  }

--------------------------------------------------------------------------------
-- | Create a 'User'.
peter :: User
peter = User { firstName    = "Peter"
             , lastName     = "Jones"
             , shoeSize     = 8.0
             , likesHaskell = True
             }

--------------------------------------------------------------------------------
-- | Calculate a full name from first and last using field functions.
fullName :: User -> String
fullName user = firstName user ++ " " ++ lastName user

--------------------------------------------------------------------------------
-- | Record update syntax (Haskell wart).
bumpShoeSize :: User -> User
bumpShoeSize user = user {shoeSize = shoeSize user + 0.5}

-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn (fullName peter)
