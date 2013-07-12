module Main where

-- {BEGIN: product}

--------------------------------------------------------------------------------
-- | User type without record syntax.
data User = User String String Float Bool

--------------------------------------------------------------------------------
-- | Create a user.
peter :: User
peter = User "Peter" "Jones" 8.0 True

--------------------------------------------------------------------------------
-- | Pattern match to get the first name.
firstName :: User -> String
firstName (User x _ _ _) = x

--------------------------------------------------------------------------------
-- | Pattern match to create a full name.
fullName :: User -> String
fullName (User fname lname _ _) = fname ++ " " ++ lname

-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn (firstName peter)
