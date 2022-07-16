{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Database.PostgreSQL.Simple
import Db.Configure
import Operations.InterfaceOperations
import Operations.MovieOperations
import System.IO
import System.Exit
import Operations.SerieOperations (getSeriesWithRatings, updateAllSerieRating)
import qualified System.Process as System
import Operations.UtilOperations (clearScreenOnly, clearScreenWithConfirmation)


main :: IO ()
main = do
    clearScreenOnly
    putStrLn ""
    putStrLn "Conectando com a base de dados... (Aguarde)"
    conn <- connectToDB
    clearScreenOnly
    putStrLn "Recuperando informações sobre filmes... (Aguarde)"
    movies <- getMoviesWithRatings conn
    clearScreenOnly
    putStrLn "Recuperando informações sobre séries... (Aguarde)"
    series <- getSeriesWithRatings conn
    clearScreenOnly
    putStrLn "Atualizando informações sobre filmes... (Aguarde)"
    updateAllMovieRating conn movies
    clearScreenOnly
    putStrLn "Atualizando informações sobre séries... (Aguarde)"
    updateAllSerieRating conn series
    clearScreenOnly
    putStrLn "Tudo pronto!"
    clearScreenWithConfirmation
    firstMenu conn
    exitSuccess





