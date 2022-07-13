{-# LANGUAGE OverloadedStrings #-}
module Db.Operations where

import Database.PostgreSQL.Simple
import Db.Configure
import Entities.User
import qualified Entities.Movie as M
import qualified Entities.Rating as R
import Data.List
import Data.Time

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

avaluateMovie :: Connection -> User -> M.Movie -> Integer -> String -> IO Bool
avaluateMovie conn user movie rating commentary = do
    userExists <- userAlreadyExists conn $ email user
    if userExists && (rating `elem` [1..5]) then do
        execute conn "INSERT INTO ratings (useremail, movieid, rating, commentary) values (?, ?, ?, ?)" (email user, M.movieId movie, rating, commentary)
        return True
    else do
        return False
        

getRatings :: Connection -> M.Movie -> IO [R.Rating]
getRatings conn movie = do
    query conn "SELECT * FROM ratings WHERE movieid = ?;" [M.movieId movie] :: IO [R.Rating]


getCasting :: Connection -> M.Movie -> IO [String]
getCasting conn movie = undefined



titleContainsWord :: [String] -> M.Movie -> Bool
titleContainsWord [] _ = False
titleContainsWord (x:xs) movie = x `isInfixOf` M.title movie || titleContainsWord xs movie


filterMovies :: [M.Movie] -> [String] -> [M.Movie]
filterMovies [] _ = []
filterMovies (x:xs) names = if titleContainsWord names x then x:filterMovies xs names else filterMovies xs names


searchMovie :: Connection -> String -> IO [M.Movie]
searchMovie conn title = do
    let qWords = words title
    movies <- query_ conn "SELECT * FROM movies m;" :: IO [M.Movie]
    return (filterMovies movies qWords)


getPlataformToWatch :: Connection -> M.Movie -> IO [String]
getPlataformToWatch conn movie = undefined


getWatchLaterList :: Connection -> User -> IO [M.Movie]
getWatchLaterList conn user = do
    movies <- query conn "SELECT * FROM watchlaterlist w WHERE w.useremail = ?" [email user] :: IO [M.Movie]
    return movies

