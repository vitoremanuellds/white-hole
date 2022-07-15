module Operations.SerieInterfaceOperations where

import Database.PostgreSQL.Simple
import Entities.Serie
import Operations.SerieOperations
import Operations.UtilOperations


createSerie :: Connection -> IO ()
createSerie conn = do
    putStrLn ""
    putStrLn "Qual o título da série?"
    title <- getLine
    if null title then
        putStrLn "A série deve ter um título!" >> createSerie conn
    else
        putStrLn "Qual a data de lançamento da série? (yyyy-mm-dd)"
    releaseDate <- getLine
    putStrLn "Qual a quantidade de episódios da série?"
    duration <- getLine
    putStrLn "Qual a sinopse da série?"
    summaryT <- getLine
    putStrLn "Digite o nome dos diretores (separados por vírgula):"
    directors <- getLine
    putStrLn "Digite o nome dos atores (separados por vírgula):"
    actors <- getLine
    let summary = if summaryT == "" then "No comments!" else summaryT
    serie <- registerSerie conn title releaseDate duration summary
    addCastingToSerie conn serie (splitItems actors [])
    addDirectorsToSerie conn serie (splitItems directors [])
    putStrLn ""
    putStrLn "Série cadastrado com sucesso!"


tenBestSeries :: Connection -> IO ()
tenBestSeries conn = undefined


tenBestSeriesByCategory :: Connection -> IO ()
tenBestSeriesByCategory conn = undefined


showSerie :: Connection -> Serie -> IO()
showSerie conn serie = undefined