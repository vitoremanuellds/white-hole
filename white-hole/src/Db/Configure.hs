{-# LANGUAGE OverloadedStrings #-}
module Db.Configure
( connectionMyDB,
  connectToDB
) where

import Database.PostgreSQL.Simple

connectToDB :: IO Connection
connectToDB = do
    putStrLn "Digite o host: "
    host <- getLine
    putStrLn "Digite o database: "
    database <- getLine
    putStrLn "Digite o user: "
    user <- getLine
    putStrLn "Digite o password: "
    password <- getLine
    let db = defaultConnectInfo {
        connectHost = host,
        connectDatabase = database,
        connectUser = user,
        connectPassword = password,
        connectPort = 5432
    }
    connect db