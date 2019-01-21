{-# LANGUAGE FlexibleInstances #-}

module Console (log, read) where

import Prelude hiding (log, read)


class Logger a where
    printToLog :: a -> IO () 

instance  Logger String where
    printToLog = putStrLn

instance Logger [String] where
    printToLog = mapM_ putStrLn



log :: (Logger a) => a -> IO ()
log = printToLog

read :: IO String
read = getLine