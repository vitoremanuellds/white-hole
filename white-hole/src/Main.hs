{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Database.PostgreSQL.Simple
import Db.Configure
import Operations.InterfaceOperations
import Operations.MovieOperations
import System.IO
import System.Exit


main :: IO ()
main = do
    conn <- connectToDB
    movies <- getMoviesWithRatings conn
    updateAllMovieRating conn movies
    firstMenu conn
    exitSuccess





