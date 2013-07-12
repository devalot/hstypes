module Main where
import Prelude (Int, String)
import System.IO

-- {BEGIN: sum}

--------------------------------------------------------------------------------
-- | The actual 'Bool' type!  Sum type: more than one data constructor.
data Bool = False | True

--------------------------------------------------------------------------------
-- | Pattern matching with all of the data constructors.
mySQLBool :: Bool -> Int
mySQLBool False = 0
mySQLBool True  = 1

--------------------------------------------------------------------------------
-- | Using the data constructors.
isPeter :: String -> Bool
isPeter "Peter" = True
isPeter _       = False

-- {END}
-- {BEGIN: day}

--------------------------------------------------------------------------------
-- | Another simple example.  Similar to an enum in C, but type safe.
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

-- {END}
-- {BEGIN: shell}

--------------------------------------------------------------------------------
-- | Example with a field.
data ShellReturn = Success | Error Int

--------------------------------------------------------------------------------
-- | Another pattern matching example.
shellSuccess :: ShellReturn -> Bool
shellSuccess Success   = True
shellSuccess (Error _) = False

-- {END}

--------------------------------------------------------------------------------
main :: IO ()
main = print (mySQLBool True)
