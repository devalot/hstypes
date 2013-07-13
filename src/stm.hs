{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- | A networked transactional counter cache implementation using only
-- libraries that come with the Haskell Platform.
--
-- Usage:
--
--   1. Start the program (runhaskell stm.hs)
--
--   2. Connect with a tool like netcat.
--
--   3. Issue commands.
--
-- Commands:
--
--  * inc <name>
--  * dec <name>
--  * get <name>
--  * del <name>
--
module Main where

--------------------------------------------------------------------------------
import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Map.Strict as Map
import Data.Monoid
import Network
import System.IO

-- {BEGIN: types}

--------------------------------------------------------------------------------
-- | Software Transactional Memory Functions:
--
-- @
--
-- atomically :: STM a -> IO a
--
-- newTVarIO  :: a -> IO (TVar a)
--
-- readTVar   :: TVar a -> STM a
--
-- readTVarIO :: TVar a -> IO a
--
-- writeTVar  :: TVar a -> a -> STM ()
--
-- modifyTVar :: TVar a -> (a -> a) -> STM ()
--
-- @
--
--------------------------------------------------------------------------------

-- {END}
-- {BEGIN: counters}

--------------------------------------------------------------------------------
-- | Type synonym for a map of @Integer@ counters with @ByteString@ keys.
type Counters = Map.Map ByteString Integer

--------------------------------------------------------------------------------
-- | Update a counter value given its key and a function.
modify :: (Integer -> Integer) -> ByteString -> Counters -> Counters
modify f = Map.alter (Just . maybe initial f) where initial = f 0

--------------------------------------------------------------------------------
-- | Mutate counters on behalf of a client.
worker :: Handle -> TVar Counters -> IO ()
worker h counters = forever $ do
  (cmd, key) <- B.splitAt 4 `fmap` B.hGetLine h
  putStrLn $ "CMD: " ++ show cmd ++ " KEY: " ++ show key

  atomically $ case cmd of
    "inc " -> modifyTVar' counters (modify succ key)
    "dec " -> modifyTVar' counters (modify pred key)
    "del " -> modifyTVar' counters (Map.delete key)
    _      -> return ()

  val <- Map.lookup key `fmap` readTVarIO counters

  B.hPutBuilder h $ case val of
    Nothing -> B.byteString "0\n"
    Just n  -> B.string8 (show n) <> B.char8 '\n'

--------------------------------------------------------------------------------
-- | Listen on a port and fork threads for each connection.
main :: IO ()
main = withSocketsDo $ do
  counters <- newTVarIO Map.empty
  sock     <- listenOn (PortNumber 9090)

  forever $ do
    (handle, _, _) <- accept sock
    hSetBuffering handle LineBuffering
    forkFinally (worker handle counters) (\_ -> hClose handle)

-- {END}
