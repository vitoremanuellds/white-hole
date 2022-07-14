{-# LANGUAGE OverloadedStrings #-}
module Db.Operations where

import Database.PostgreSQL.Simple
import Db.Configure
import Entities.User
import qualified Entities.Movie as M
import qualified Entities.Rating as R
import qualified Entities.Serie as S
import Data.List
import Data.Char (toLower)
import Text.Read (Lexeme(String))


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


getCasting :: Connection -> M.Movie -> IO [(String, String)]
getCasting conn movie = do
    query conn "SELECT name, movieFunction FROM casting WHERE movieid = ?;" [M.movieId movie] :: IO [(String, String)]

getCastingSerie :: Connection -> S.Serie -> IO [(String, String)]
getCastingSerie conn serie = do
    query conn "SELECT name, seriefunction FROM seriescasting WHERE serieid = ?;" [S.serieId serie] :: IO [(String, String)]

titleContainsWordMovie :: [String] -> M.Movie -> Bool
titleContainsWordMovie [] _ = False
titleContainsWordMovie (x:xs) movie = x `isInfixOf` map toLower (M.title movie) || titleContainsWordMovie xs movie


filterMovies :: [M.Movie] -> [String] -> [M.Movie]
filterMovies [] _ = []
filterMovies (x:xs) names = if titleContainsWordMovie names x then x:filterMovies xs names else filterMovies xs names


searchMovie :: Connection -> String -> IO [M.Movie]
searchMovie conn title = do
    let qWords = words title
    movies <- query_ conn "SELECT * FROM movies m;" :: IO [M.Movie]
    return (filterMovies movies qWords)

titleContainsWordSerie :: [String] -> S.Serie -> Bool
titleContainsWordSerie [] _ = False
titleContainsWordSerie (x:xs) serie = x `isInfixOf` map toLower (S.title serie) || titleContainsWordSerie xs serie

filterSeries :: [S.Serie] -> [String] -> [S.Serie]
filterSeries [] _ = []
filterSeries (x:xs) names = if titleContainsWordSerie names x then x:filterSeries xs names else filterSeries xs names

searchSerie :: Connection -> String -> IO [S.Serie]
searchSerie conn title = do
    let qWords = words title
    series <- query_ conn "SELECT * FROM series s;" :: IO [S.Serie]
    return (filterSeries series qWords)


getPlataformToWatchMovies :: Connection -> M.Movie -> IO [String]
getPlataformToWatchMovies conn movie = undefined

getPlataformToWatchSeries :: Connection -> S.Serie -> IO [String]
getPlataformToWatchSeries conn serie = undefined


getWatchLaterList :: Connection -> User -> IO [M.Movie]
getWatchLaterList conn user = do
    query conn "SELECT * FROM watchlaterlist w WHERE w.useremail = ?" [email user] :: IO [M.Movie]


registerMovie :: Connection -> String -> String -> String -> String -> IO M.Movie
registerMovie conn title releaseDate summary duration = do
    execute conn "INSERT INTO movies(title, releasedate, duration, summary) VALUES (?,?,?,?)" (title, releaseDate, summary, duration)
    movie <- query conn "SELECT * FROM movies WHERE title=?;" [title] :: IO [M.Movie]
    return (head movie)


registerSerie :: Connection -> String -> String -> String -> String -> IO S.Serie
registerSerie conn title releaseDate episodes summary = do
    execute conn "INSERT INTO series(title, releasedate, episodes, summary) VALUES (?,?,?,?);" (title, releaseDate, episodes, summary)
    serie <- query conn "SELECT * FROM series WHERE title=?;" [title] :: IO [S.Serie]
    return (head serie)


addCastingToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addCastingToMovie conn movie [] = return True
addCastingToMovie conn movie actors = do
    execute conn "INSERT INTO casting VALUES (?,?, 'actor')" (M.movieId movie, head actors)
    addCastingToMovie conn movie (tail actors)


addCastingToSerie :: Connection -> S.Serie -> [String] -> IO Bool
addCastingToSerie conn serie [] = return True
addCastingToSerie conn serie actors = do
    execute conn "INSERT INTO seriescasting VALUES (?,?, 'actor')" (S.serieId serie, head actors)
    addCastingToSerie conn serie (tail actors)


addDirectorsToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addDirectorsToMovie conn movie [] = return True
addDirectorsToMovie conn movie directors = do
    execute conn "INSERT INTO casting VALUES (?,?, 'director')" (M.movieId movie, head directors)
    addDirectorsToMovie conn movie (tail directors)


addDirectorsToSerie :: Connection -> S.Serie -> [String] -> IO Bool
addDirectorsToSerie conn serie [] = return True
addDirectorsToSerie conn serie directors = do
    execute conn "INSERT INTO seriescasting VALUES (?,?, 'director')" (S.serieId serie, head directors)
    addDirectorsToSerie conn serie (tail directors)


updateMovieRating :: Connection -> M.Movie -> IO Float
updateMovieRating conn movie = do
    [Only rating] <- query conn "SELECT AVG(rating) FROM ratings WHERE movieid=?;" [M.movieId movie] :: IO [Only Float]
    execute conn "UPDATE movies SET rating = ? WHERE movieid = ?;" (rating, M.movieId movie)
    return rating

updateSerieRating :: Connection -> S.Serie -> IO Float
updateSerieRating conn serie = do
    [Only rating] <- query conn "SELECT AVG(rating) FROM seriesratings WHERE serieid=?;" [S.serieId serie] :: IO [Only Float]
    execute conn "UPDATE series SET rating = ? WHERE serieid = ?;" (rating, S.serieId serie)
    return rating

