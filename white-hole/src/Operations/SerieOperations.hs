{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Operations.SerieOperations where

import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Serie as S
import qualified Entities.Rating as R
import Operations.UserOperations (userAlreadyExists, addToWatchLaterList)
import Data.List
import Data.Char (toLower)
import Operations.UtilOperations

avaluateSerie :: Connection -> User -> S.Serie -> Integer -> String -> IO Bool
avaluateSerie conn user serie rating commentary = do
    userExists <- userAlreadyExists conn $ email user
    if userExists && (rating `elem` [1..5]) then do
        execute conn "INSERT INTO seriesratings (useremail, serieid, rating, commentary) values (?, ?, ?, ?)" (email user, S.serieId serie, rating, commentary)
        return True
    else do
        return False



getRatingsSeries :: Connection -> S.Serie -> IO [R.Rating]
getRatingsSeries conn serie = do
    query conn "SELECT * FROM seriesratings WHERE serieid = ?;" [S.serieId serie] :: IO [R.Rating]


getCastingSerie :: Connection -> S.Serie -> IO [(String, String)]
getCastingSerie conn serie = do
    query conn "SELECT name, seriefunction FROM seriescasting WHERE seriesid = ?;" [S.serieId serie] :: IO [(String, String)]


titleContainsWordSerie :: [String] -> S.Serie -> Bool
titleContainsWordSerie [] _ = False
titleContainsWordSerie (x:xs) serie = (x `elem` words (map toLower (S.title serie)) || ((x `isInfixOf` map toLower (S.title serie)) && (length x > 2))) || titleContainsWordSerie xs serie


filterSeries :: [S.Serie] -> [String] -> [S.Serie]
filterSeries [] _ = []
filterSeries (x:xs) names = if titleContainsWordSerie names x then x:filterSeries xs names else filterSeries xs names

getSerieWithExactTitle :: [S.Serie] -> String -> [S.Serie]
getSerieWithExactTitle [] title = []
getSerieWithExactTitle (x:xs) title = if S.title x == title then [x] else getSerieWithExactTitle xs title


searchSerie :: Connection -> String -> IO [S.Serie]
searchSerie conn title = do
    let qWords = words title
    series <- query_ conn "SELECT * FROM series s;" :: IO [S.Serie]
    let serie = getSerieWithExactTitle series title
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


showSerieRating :: Connection -> S.Serie -> IO [(Integer, Integer)]
showSerieRating conn serie = do
    query conn "SELECT SUM(rating), COUNT(rating) FROM seriesratings WHERE serieid=?;" [S.serieId serie] :: IO [(Integer, Integer)]



updateSerieRating :: Connection -> S.Serie -> IO Float
updateSerieRating conn serie = do
    ratings <- showSerieRating conn serie
    let q = fromIntegral (fst $ head ratings) / fromIntegral (snd $ head ratings)
    execute conn "UPDATE series SET rating = ? WHERE seriesid = ?;" (q :: Float, S.serieId serie)
    return q


updateAllSerieRating :: Connection -> [S.Serie] -> IO ()
updateAllSerieRating conn [] = return ()
updateAllSerieRating conn (x:xs) = do
    rating <- updateSerieRating conn x
    updateAllSerieRating conn xs


getSeriesWithRatings :: Connection -> IO [S.Serie]
getSeriesWithRatings conn = do
    seriesWithRating <- query_ conn "SELECT serieid, count(serieid) FROM seriesratings group by serieid ;" :: IO [(Integer, Integer)]
    getSeriesById conn (map fst seriesWithRating) []


getSeriesByCategory :: Connection -> String -> IO [S.Serie]
getSeriesByCategory conn category = do
    query conn "select serieid, title, releasedate, episodes, summary, rating from ((select seriesid as sid, category  from seriescategories c where category = ?) as c join series s on c.sid = s.seriesid) order by rating desc limit 10;" [category] :: IO [S.Serie]
    
    

getSeriesById :: Connection -> [Integer] -> [S.Serie] -> IO [S.Serie]
getSeriesById conn [] result = return result
getSeriesById conn (x:xs) result = do
    serie <- query conn "SELECT * FROM series WHERE seriesid = ?" [x] :: IO [S.Serie]
    getSeriesById conn xs (head serie:result)
    


getCategoriesOfSeriesInOneString :: Connection -> S.Serie -> IO String
getCategoriesOfSeriesInOneString conn serie = do
    categories <- query conn "SELECT * FROM seriescategories WHERE seriesid = ?;" [S.serieId serie] :: IO [(Integer, String)]
    let result = if not (null categories) then init $ concatenateWithComma (map snd categories) else ""
    return result


addCategoriesToSerie :: Connection -> S.Serie -> [String] -> IO Bool
addCategoriesToSerie conn serie [] = return True
addCategoriesToSerie conn serie categories = do
    execute conn "INSERT INTO seriescategories VALUES (?,?)" (S.serieId serie, head categories)
    addCategoriesToSerie conn serie (tail categories)


getRecomendationsOfSeries :: Connection -> User -> IO [S.Serie]
getRecomendationsOfSeries conn user = do
    users <- getUsersWhoAvaluateSeries conn
    avaluateRecomendationsSeries conn users user


avaluateRecomendationsSeries :: Connection -> [User] -> User -> IO [S.Serie]
avaluateRecomendationsSeries conn [] user = return []
avaluateRecomendationsSeries conn (x:xs) user = do
    series <- getSeriesAvaluatedByUser conn x
    mySeries <- getSeriesAvaluatedByUser conn user
    let alike = series `intersect` mySeries
    if length alike > (length mySeries `div` 2) + 1 then do
        nexts <- avaluateRecomendationsSeries conn xs user
        return ((series \\ mySeries) ++ nexts)
    else do
        avaluateRecomendationsSeries conn xs user



getAvaluationsSeries :: Connection -> User -> IO [R.Rating]
getAvaluationsSeries conn user = do
    query conn "select * from seriesratings r where useremail = ?;" [email user] :: IO [R.Rating]


getSeriesAvaluatedByUser :: Connection -> User -> IO [S.Serie]
getSeriesAvaluatedByUser conn user = do
    seriesId <- query conn "select serieid, ratingid from seriesratings r where useremail = ?;" [email user] :: IO [(Integer, Integer)]
    getSeriesById conn (map fst seriesId) []


getUsersWhoAvaluateSeries :: Connection -> IO [User]
getUsersWhoAvaluateSeries conn = do
    query_ conn "select useremail from seriesratings r group by useremail;" :: IO [User]


getWatchLaterListSeries :: Connection -> User -> IO [S.Serie]
getWatchLaterListSeries conn user = do
    query conn "SELECT m.seriesid, m.title, m.releasedate, m.episodes, m.summary, m.rating FROM (watchlaterlistseries w JOIN series s ON w.serieid=s.seriesid) WHERE w.useremail = ?;" [email user] :: IO [S.Serie]