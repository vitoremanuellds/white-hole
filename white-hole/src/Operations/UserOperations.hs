{-# LANGUAGE OverloadedStrings #-}
module Operations.UserOperations where

import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Movie as M
import qualified Entities.Rating as R
import qualified Entities.Serie as S


createUser :: Connection -> User -> IO Bool
createUser conn user = do
    exists <- userAlreadyExists conn $ email user
    if not exists then do
        ok <- execute conn "INSERT INTO users (email, passwd, nome, sobrenome) values (?,?,?,?);" (email user, password user, nome user, sobrenome user)
        return True
    else return False


getUserByEmail :: Connection -> String -> IO [User]
getUserByEmail conn userEmail = do
    let q = "SELECT * FROM users WHERE email = ?;"
    query conn q [userEmail] :: IO [User]


userAlreadyExists :: Connection -> String -> IO Bool
userAlreadyExists conn userEmail = do
    result <- getUserByEmail conn userEmail
    return (not (null result))


authenticate :: Connection -> String -> String -> IO Bool
authenticate conn userEmail userPassword = do
    result <- getUserByEmail conn userEmail
    if not (null result) then do
        return (password (head result) == userPassword)
    else
        return False


addToWatchLaterList :: Connection -> User -> M.Movie -> IO Bool
addToWatchLaterList conn user movie = do
    userExists <- userAlreadyExists conn $ email user
    if userExists then do
        execute conn "INSERT INTO watchlaterlist values (?,?)" (email user, M.movieId movie)
        return True
    else do
        return False