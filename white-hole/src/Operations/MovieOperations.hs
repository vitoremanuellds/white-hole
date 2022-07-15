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
import Operations.UtilOperations (isANumber)


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


updateMovieRating :: Connection -> M.Movie -> IO Float
updateMovieRating conn movie = do
    [Only rating] <- query conn "SELECT AVG(rating) FROM ratings WHERE movieid=?;" [M.movieId movie] :: IO [Only Float]
    execute conn "UPDATE movies SET rating = ? WHERE movieid = ?;" (rating, M.movieId movie)
    return rating


getWatchLaterList :: Connection -> User -> IO [M.Movie]
getWatchLaterList conn user = do
    query conn "SELECT m.movieid, m.title, m.releasedate, m.duration, m.summary, m.rating FROM (watchlaterlist w JOIN movies m ON w.movieid=m.movieid) WHERE w.useremail = ?;" [email user] :: IO [M.Movie]


getCategoriesOfMoviesInOneString :: Connection -> M.Movie -> IO String
getCategoriesOfMoviesInOneString conn movie = do
    categories <- query conn "SELECT * FROM categories WHERE movieid = ?;" [M.movieId movie] :: IO [(Integer, String)]
    let result = init $ concatenateWithComma (map snd categories)
    return result


concatenateWithComma :: [String] -> String
concatenateWithComma [] = []
concatenateWithComma (x:xs) = " " ++ x ++ "," ++ concatenateWithComma xs


addCategoriesToMovie :: Connection -> M.Movie -> [String] -> IO Bool
addCategoriesToMovie conn movie [] = return True
addCategoriesToMovie conn movie categories = do
    execute conn "INSERT INTO categories VALUES (?,?)" (M.movieId movie, head categories)
    addDirectorsToMovie conn movie (tail categories)


verifyCategories :: [String] -> IO Bool
verifyCategories [] = return True
verifyCategories (x:xs) = do
    result <- verifyCategories xs
    return (isANumber x True && (read x :: Integer) `elem` [1..14] && result)


convertCategories :: [String] -> [String] -> [String]
convertCategories [] result = reverse result
convertCategories (x:xs) result = do
    let categories = ["ação","suspense","romance","comédia","terror","aventura","investigação","fantasia","documentário","drama","anime","mistério","infantil","ficção científica"]
    convertCategories xs (categories !! ((read x :: Int) - 1):result)



    