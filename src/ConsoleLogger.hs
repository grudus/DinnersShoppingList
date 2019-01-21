{-# LANGUAGE FlexibleInstances #-}

module ConsoleLogger (log) where

import Prelude hiding (log)


class Logger a where
    printToLog :: a -> IO () 

instance  Logger String where
    printToLog = putStrLn

instance Logger [String] where
    printToLog = mapM_ putStrLn


log :: (Logger a) => a -> IO ()
log x = printToLog x