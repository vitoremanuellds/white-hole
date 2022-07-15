{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Database.PostgreSQL.Simple
import Db.Configure
import Operations.InterfaceOperations
import Operations.MovieOperations
import System.IO
import System.Exit
import Operations.SerieOperations (getSeriesWithRatings, updateAllSerieRating)


main :: IO ()
main = do
    putStrLn ""
    putStrLn "Conectando com a base de dados... (Aguarde)"
    conn <- connectToDB
    putStrLn ""
    putStrLn "Recuperando informações sobre filmes... (Aguarde)"
    movies <- getMoviesWithRatings conn
    putStrLn ""
    putStrLn "Recuperando informações sobre séries... (Aguarde)"
    series <- getSeriesWithRatings conn
    putStrLn ""
    putStrLn "Atualizando informações sobre filmes... (Aguarde)"
    updateAllMovieRating conn movies
    putStrLn ""
    putStrLn "Atualizando informações sobre séries... (Aguarde)"
    updateAllSerieRating conn series
    putStrLn ""
    putStrLn "Tudo pronto!"
    firstMenu conn
    exitSuccess





