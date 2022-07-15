{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Operations.MovieOperations where
import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Movie as M
import qualified Entities.Rating as R
import Operations.UserOperations (userAlreadyExists)
import Data.List
import Data.Char (toLower, isDigit)
import Text.Read (Lexeme(String))
import Operations.UtilOperations (isANumber, concatenateWithComma)
import GHC.RTS.Flags (TraceFlags(user))


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


titleContainsWordMovie :: [String] -> M.Movie -> Bool
titleContainsWordMovie [] _ = False
titleContainsWordMovie (x:xs) movie = (x `elem` words (map toLower (M.title movie)) || ((x `isInfixOf` map toLower (M.title movie)) && (length x > 2))) || titleContainsWordMovie xs movie


filterMovies :: [M.Movie] -> [String] -> [M.Movie] -> [M.Movie]
filterMovies [] _ result = reverse result
filterMovies (x:xs) names result = if titleContainsWordMovie names x then filterMovies xs names (x:result) else filterMovies xs names result


getMovieWithExactTitle :: [M.Movie] -> String -> [M.Movie]
getMovieWithExactTitle [] title = []
getMovieWithExactTitle (x:xs) title = if M.title x == title then [x] else getMovieWithExactTitle xs title



searchMovie :: Connection -> String -> IO [M.Movie]
searchMovie conn title = do
    let qWords = words title
    movies <- query_ conn "SELECT * FROM movies m;" :: IO [M.Movie]
    let movie = getMovieWithExactTitle movies title
    return (filterMovies (movies \\ movie) qWords movie)


getPlataformToWatchMovies :: Connection -> M.Movie -> IO [String]
getPlataformToWatchMovies conn movie = undefined


registerMovie :: Connection -> String -> String -> String -> String -> IO M.Movie
registerMovie conn title releaseDate summary duration = do
    execute conn "INSERT INTO movies(title, releasedate, duration, summary) VALUES (?,?,?,?)" (title, releaseDate, summary, duration)
    movie <- query conn "SELECT * FROM movies WHERE title=?;" [title] :: IO [M.Movie]
    return (head movie)


addCastingToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addCastingToMovie conn movie [] = return True
addCastingToMovie conn movie actors = do
    execute conn "INSERT INTO casting VALUES (?,?, 'actor')" (M.movieId movie, head actors)
    addCastingToMovie conn movie (tail actors)


addDirectorsToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addDirectorsToMovie conn movie [] = return True
addDirectorsToMovie conn movie directors = do
    execute conn "INSERT INTO casting VALUES (?,?, 'director')" (M.movieId movie, head directors)
    addDirectorsToMovie conn movie (tail directors)


showMovieRating :: Connection -> M.Movie -> IO [(Integer, Integer)]
showMovieRating conn movie = do
    query conn "SELECT SUM(rating), COUNT(rating) FROM ratings WHERE movieid=?;" [M.movieId movie] :: IO [(Integer, Integer)]


updateMovieRating :: Connection -> M.Movie -> IO Float
updateMovieRating conn movie = do
    ratings <- showMovieRating conn movie
    let q = fromIntegral (fst $ head ratings) / fromIntegral (snd $ head ratings)
    execute conn "UPDATE movies SET rating = ? WHERE movieid = ?;" (q :: Float, M.movieId movie)
    return q


updateAllMovieRating :: Connection -> [M.Movie] -> IO ()
updateAllMovieRating conn [] = return ()
updateAllMovieRating conn (x:xs) = do
    rating <- updateMovieRating conn x
    updateAllMovieRating conn xs


getMoviesWithRatings :: Connection -> IO [M.Movie]
getMoviesWithRatings conn = do
    moviesWithRating <- query_ conn "SELECT movieid, count(movieid) FROM ratings group by movieid ;" :: IO [(Integer, Integer)]
    getMoviesById conn (map fst moviesWithRating) []


getMoviesByCategory :: Connection -> String -> IO [M.Movie]
getMoviesByCategory conn category = do
    query conn "select movieid, title, releasedate, duration, summary, rating from ((select movieid as mid, category  from categories c where category = ?) as c join movies m on c.mid = m.movieid) order by rating desc limit 10;" [category] :: IO [M.Movie]



getMoviesById :: Connection -> [Integer] -> [M.Movie] -> IO [M.Movie]
getMoviesById conn [] result = return result
getMoviesById conn (x:xs) result = do
    movie <- query conn "SELECT * FROM movies WHERE movieid = ?" [x] :: IO [M.Movie]
    getMoviesById conn xs (head movie:result)



getWatchLaterList :: Connection -> User -> IO [M.Movie]
getWatchLaterList conn user = do
    query conn "SELECT m.movieid, m.title, m.releasedate, m.duration, m.summary, m.rating FROM (watchlaterlist w JOIN movies m ON w.movieid=m.movieid) WHERE w.useremail = ?;" [email user] :: IO [M.Movie]


getCategoriesOfMoviesInOneString :: Connection -> M.Movie -> IO String
getCategoriesOfMoviesInOneString conn movie = do
    categories <- query conn "SELECT * FROM categories WHERE movieid = ?;" [M.movieId movie] :: IO [(Integer, String)]
    let result = if not (null categories) then init $ concatenateWithComma (map snd categories) else ""
    return result


addCategoriesToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addCategoriesToMovie conn movie [] = return True
addCategoriesToMovie conn movie categories = do
    execute conn "INSERT INTO categories VALUES (?,?)" (M.movieId movie, head categories)
    addCategoriesToMovie conn movie (tail categories)


getRecomendationsOfMovies :: Connection -> User -> IO [M.Movie]
getRecomendationsOfMovies conn user = do
    users <- getUsersWhoAvaluate conn
    avaluateRecomendations conn users user


avaluateRecomendations :: Connection -> [User] -> User -> IO [M.Movie]
avaluateRecomendations conn [] user = return []
avaluateRecomendations conn (x:xs) user = do
    movies <- getMoviesAvaluatedByUser conn x
    myMovies <- getMoviesAvaluatedByUser conn user
    let alike = movies `intersect` myMovies
    if length alike > (length myMovies `div` 2) + 1 then do
        nexts <- avaluateRecomendations conn xs user
        return ((movies \\ myMovies) ++ nexts)
    else do
        avaluateRecomendations conn xs user



getAvaluations :: Connection -> User -> IO [R.Rating]
getAvaluations conn user = do
    query conn "select * from ratings r where useremail = ?;" [email user] :: IO [R.Rating]


getMoviesAvaluatedByUser :: Connection -> User -> IO [M.Movie]
getMoviesAvaluatedByUser conn user = do
    moviesId <- query conn "select movieid, ratingid from ratings r where useremail = ?;" [email user] :: IO [(Integer, Integer)]
    getMoviesById conn (map fst moviesId) []


getUsersWhoAvaluate :: Connection -> IO [User]
getUsersWhoAvaluate conn = do
    query_ conn "select useremail  from ratings r group by useremail;" :: IO [User]