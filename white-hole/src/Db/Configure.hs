{-# LANGUAGE OverloadedStrings #-}
module Db.Configure
( connectToDB
) where

import Database.PostgreSQL.Simple
import System.Environment

connectToDB :: IO Connection
connectToDB = do
    host <- getEnv "dbhost"
    database <- getEnv "db"
    user <- getEnv "dbuser"
    password <- getEnv "dbpassword"
    let db = defaultConnectInfo {
        connectHost = host,
        connectDatabase = database,
        connectUser = user,
        connectPassword = password,
        connectPort = 5432
    }
    connect db
