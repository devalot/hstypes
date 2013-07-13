module Main where

-- {BEGIN: sql}

--------------------------------------------------------------------------------
-- | Phantom type to hold potentially dangerous query.  In a real
-- library the data constructor would not be exported.
data SQL a = SQL String

--------------------------------------------------------------------------------
-- | Empty types to track the state of a query.
data Tainted
data Sanitized

--------------------------------------------------------------------------------
-- | Smart constructor that forces a query to be tagged as tainted.
makeSQL :: String -> SQL Tainted
makeSQL = SQL

--------------------------------------------------------------------------------
-- | Function to allow modifying a query, forcing it to become tainted.
modify :: (String -> String) -> SQL a -> SQL Tainted
modify f (SQL x) = SQL (f x)

--------------------------------------------------------------------------------
-- | Sanitize the given query.
sanitize :: SQL a -> SQL Sanitized
sanitize = undefined

--------------------------------------------------------------------------------
-- | Execute a sanitized query and return the number of matching rows.
execute :: SQL Sanitized -> Int
execute = undefined

-- {END}
--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn "HW"
