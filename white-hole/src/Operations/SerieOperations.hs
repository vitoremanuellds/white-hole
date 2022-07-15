{-# LANGUAGE OverloadedStrings #-}
module Operations.SerieOperations where

import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Serie as S
import qualified Entities.Rating as R
import Operations.UserOperations (userAlreadyExists)
import Data.List
import Data.Char (toLower)


getCastingSerie :: Connection -> S.Serie -> IO [(String, String)]
getCastingSerie conn serie = do
    query conn "SELECT name, seriefunction FROM seriescasting WHERE serieid = ?;" [S.serieId serie] :: IO [(String, String)]


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


getPlataformToWatchSeries :: Connection -> S.Serie -> IO [String]
getPlataformToWatchSeries conn serie = undefined


registerSerie :: Connection -> String -> String -> String -> String -> IO S.Serie
registerSerie conn title releaseDate episodes summary = do
    execute conn "INSERT INTO series(title, releasedate, episodes, summary) VALUES (?,?,?,?);" (title, releaseDate, episodes, summary)
    serie <- query conn "SELECT * FROM series WHERE title=?;" [title] :: IO [S.Serie]
    return (head serie)


addCastingToSerie :: Connection -> S.Serie -> [String] -> IO Bool
addCastingToSerie conn serie [] = return True
addCastingToSerie conn serie actors = do
    execute conn "INSERT INTO seriescasting VALUES (?,?, 'actor')" (S.serieId serie, head actors)
    addCastingToSerie conn serie (tail actors)


addDirectorsToSerie :: Connection -> S.Serie -> [String] -> IO Bool
addDirectorsToSerie conn serie [] = return True
addDirectorsToSerie conn serie directors = do
    execute conn "INSERT INTO seriescasting VALUES (?,?, 'director')" (S.serieId serie, head directors)
    addDirectorsToSerie conn serie (tail directors)


updateSerieRating :: Connection -> S.Serie -> IO Float
updateSerieRating conn serie = do
    [Only rating] <- query conn "SELECT AVG(rating) FROM seriesratings WHERE serieid=?;" [S.serieId serie] :: IO [Only Float]
    execute conn "UPDATE series SET rating = ? WHERE serieid = ?;" (rating, S.serieId serie)
    return rating

