{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Database.PostgreSQL.Simple
import Db.Configure
import Operations.InterfaceOperations
import System.IO
import System.Exit


main :: IO ()
main = do
    conn <- connectToDB
    firstMenu conn
    exitSuccess





