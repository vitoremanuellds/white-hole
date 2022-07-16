{-# LANGUAGE OverloadedStrings #-}
module Operations.SerieInterfaceOperations where

import Database.PostgreSQL.Simple
import Entities.Serie
import Operations.SerieOperations
import Operations.UtilOperations
import qualified Entities.Serie as S
import Entities.User
import qualified Entities.Rating as R
import Data.Char
import System.IO
import Operations.UserOperations


createSerie :: Connection -> IO ()
createSerie conn = do
    putStrLn ""
    putStrLn "Qual o título da série?"
    title <- getLine
    if null $ strip title then do
        putStrLn "A série deve ter um título!"
        clearScreenWithConfirmation
    else do
        putStrLn "Qual a data de lançamento da série? (yyyy-mm-dd)"
        releaseDate <- getLine
        if null (strip releaseDate) || length releaseDate /= 10 then do
            putStrLn "A data deve ser uma data válida!"
            clearScreenWithConfirmation
        else do 
            putStrLn "Qual a quantidade de episódios da série?"
            episodes <- getLine
            if null (strip episodes) || not (isANumber episodes True) || length episodes > 4 then do
                putStrLn "Digite uma quantidade de episódios válida"
                clearScreenWithConfirmation
            else do 
                putStrLn "Qual a sinopse da série?"

                summaryT <- getLine
                putStrLn "Digite o nome dos diretores (separados por vírgula):"
                directors <- getLine
                putStrLn "Digite o nome dos atores (separados por vírgula):"
                actors <- getLine
                let summary = if null summaryT then "Sem sinopse!" else summaryT
                putStrLn ""
                putStrLn "Digite a quais categorias essa série pertence (digite os números associados separados por espaços):"
                putStrLn ""
                putStrLn "1 - Ação          8  - Fantasia"
                putStrLn "2 - Suspense      9  - Documentário"
                putStrLn "3 - Romance       10 - Drama"
                putStrLn "4 - Comédia       11 - Anime"
                putStrLn "5 - Terror        12 - Mistério"
                putStrLn "6 - Aventura      13 - Infantil"
                putStrLn "7 - Investigação  14 - Ficção científica"
                putStrLn ""

                categoriesTemp <- getLine
                if null (strip categoriesTemp) || not (isANumber categoriesTemp True) then do
                    putStrLn "É necessário que a série tenha ao menos uma categoria!"
                    clearScreenWithConfirmation
                else do 
                    putStrLn "Quer realmente adicionar essa série? (s/N)"

                    confirmation <- getLine
                    if null confirmation || map toLower confirmation == "n" then do
                        putStrLn ""
                        putStrLn "Ok!"
                        clearScreenWithConfirmation
                    else do
                        movie <- registerSerie conn title releaseDate episodes summary
                        addCastingToSerie conn movie (splitItems actors [])
                        addDirectorsToSerie conn movie (splitItems directors [])
                        verified <- verifyCategories (words categoriesTemp)
                        let categories = if verified then convertCategories (words categoriesTemp) [] else []
                        addCategoriesToSerie conn movie categories
                        putStrLn ""
                        putStrLn "Série cadastrada com sucesso!"
                        clearScreenWithConfirmation



printSeriesList :: [S.Serie] -> Integer -> IO ()
printSeriesList [] _ = return ()
printSeriesList series seqNum = do
    putStrLn (show seqNum ++ " - " ++ capitalize (S.title (head series)))
    printSeriesList (tail series) (seqNum + 1)


tenBestSeries :: Connection -> User -> IO ()
tenBestSeries conn user = do
    putStrLn ""
    putStrLn "As dez séries mais bem avaliadas no momento são: "
    putStrLn ""
    series <- getTenBestSeries conn
    printSeriesList series 1
    putStrLn ""
    putStrLn "Digite o número da séries para acessá-la (aperte enter para voltar):"
    option <- getLine
    if null option then do
        clearScreenOnly
    else do 
        if not (isANumber option True) || (((read option :: Int) > length series) || (read option :: Int) < 1) then do
            putStrLn ""
            putStrLn "Digite uma opção válida na próxima vez."
            clearScreenWithConfirmation
        else do
            clearScreenOnly
            showSerie conn user (series !! ((read option :: Int) - 1)) >> tenBestSeries conn user


tenBestSeriesByCategory :: Connection -> User -> IO ()
tenBestSeriesByCategory conn user = do
    putStrLn ""
    putStrLn "Selecione a categoria que você quer vizualizar:"
    putStrLn ""
    putStrLn "1 - Ação          8  - Fantasia"
    putStrLn "2 - Suspense      9  - Documentário"
    putStrLn "3 - Romance       10 - Drama"
    putStrLn "4 - Comédia       11 - Anime"
    putStrLn "5 - Terror        12 - Mistério"
    putStrLn "6 - Aventura      13 - Infantil"
    putStrLn "7 - Investigação  14 - Ficção científica"
    putStrLn ""
    putStrLn "Digite o número da categoria:"
    putStrLn ""
    option <- getLine
    if null (strip option) || not (isANumber option True) || (((read option :: Int) > 14) || (read option :: Int) < 1) then do
        putStrLn ""
        putStrLn "Digite uma opção válida na próxima vez."
        clearScreenWithConfirmation
    else do 
        clearScreenOnly
        putStrLn "As dez séries mais bem avaliadas dessa categoria no momento são (Se a categoria não contiver filmes o suficiente, pode não aparecer dez séries): "
        series <- getSeriesByCategory conn $ head (convertCategories (words option) [])
        putStrLn ""
        printSeriesList series 1
        putStrLn ""
        putStrLn "Digite o número da série para acessá-la (aperte enter para voltar):"
        option <- getLine
        if null option then 
            clearScreenOnly
        else if not (isANumber option True) || (((read option :: Int) > length series) || (read option :: Int) < 1) || null option then do
            putStrLn "" >> putStrLn "Digite uma opção válida na próxima vez." >> clearScreenWithConfirmation
        else do
            clearScreenOnly >> showSerie conn user (series !! ((read option :: Int) - 1)) >> tenBestSeriesByCategory conn user


showSerie :: Connection -> User -> S.Serie -> IO ()
showSerie conn user serie = do
    putStrLn ""
    putStrLn "-------------------------------------------------"
    putStrLn ("     " ++ capitalize (S.title serie))
    putStrLn "-------------------------------------------------"
    putStrLn ""
    putStrLn ("Sinopse: " ++ S.summary serie)
    putStrLn ""
    putStrLn ("Quantidade de episódios: " ++ S.episodes serie)
    putStrLn ("Data de lançamento: " ++ S.releaseDate serie)
    categories <- getCategoriesOfSeriesInOneString conn serie
    putStrLn ("Categorias: " ++ capitalize categories)
    putStrLn ""
    putStrLn ("Nota geral: " ++ show (S.rating serie))
    putStrLn ""
    putStrLn "1 - Mostrar casting da série."
    putStrLn "2 - Marcar como assistir depois."
    putStrLn "3 - Avaliar série."
    putStrLn "4 - Mostrar avaliações da série."
    putStrLn "5 - Voltar."
    putStrLn ""
    putStrLn "Digite a opção desejada: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> clearScreenOnly >> printCasting conn serie >> showSerie conn user serie
        "2" -> clearScreenOnly >> addToWatchLaterListSeries conn user serie >> putStrLn "Filme adicionado com sucesso na lista!" >> clearScreenWithConfirmation >> showSerie conn user serie
        "3" -> clearScreenOnly >> newRating conn user serie >> updateSerieRating conn serie >> showSerie conn user serie
        "4" -> clearScreenOnly >> printRatings conn serie >> showSerie conn user serie
        "5" -> clearScreenOnly 
        x -> putStrLn "Digite uma opção válida" >> clearScreenWithConfirmation >> showSerie conn user serie



printCasting :: Connection -> S.Serie -> IO ()
printCasting conn serie = do
    casting <- getCastingSerie conn serie
    if null casting then do 
        putStrLn "" >> putStrLn "Não há um Casting cadastrado para essa série!" >> clearScreenWithConfirmation
    else do
        clearScreenOnly
        putStrLn "------------------------------------------------"
        putStrLn "                    Casting"
        putStrLn "------------------------------------------------"
        putStrLn ""
        printCasting' casting >> clearScreenWithConfirmation


printCasting' :: [(String, String)] -> IO ()
printCasting' [] = return ()
printCasting' (x:xs) = do
    putStrLn ("* " ++ fst x ++ " - " ++ capitalize (snd x))
    printCasting' xs


newRating :: Connection -> User -> S.Serie -> IO ()
newRating conn user serie = do
    putStrLn ""
    putStrLn "Dê uma nota para a série (De 1 a 5): "
    nota <- getLine
    if not (isANumber nota True && (read nota :: Int) `elem` [1..5]) then putStrLn "Digite um valor válido na próxima vez!" >> newRating conn user serie else putStrLn ""
    putStrLn "Faça um comentário sobre a série (Se não quiser, basta apertar enter): "
    commentary <- getLine
    let comment = if null commentary then "Sem comentário" else commentary
    putStrLn ""
    putStrLn "Confirmar avaliação (s/N)"
    confirmation <- getLine
    if null confirmation || map toLower confirmation == "n" then do 
        putStrLn "Ok!" 
        clearScreenWithConfirmation
    else do 
        confirmation <- avaluateSerie conn user serie (read nota :: Integer) comment
        if confirmation then do
            clearScreenOnly
            putStrLn "Avaliação feita com sucesso!"
        else do
            clearScreenOnly
            putStrLn "Um mesmo usuário não pode avaliar a mesma série duas vezes!"
        clearScreenWithConfirmation


printRatings :: Connection -> S.Serie -> IO ()
printRatings conn serie = do
    ratings <- getRatingsSeries conn serie
    if null ratings then do 
        putStrLn "" >> putStrLn "Essa série ainda não foi avaliada por algum usuário!" >> clearScreenWithConfirmation
    else do 
        putStrLn ""
        putStrLn "------------------------------------------------"
        putStrLn "                  Avaliações"
        putStrLn "------------------------------------------------"
        putStrLn ""
        printRatings' ratings >> clearScreenWithConfirmation


printRatings' :: [R.Rating] -> IO ()
printRatings' [] = return ()
printRatings' (x:xs) = do
    putStrLn (R.userEmail x ++ " - " ++ show (R.rating x) ++ " - " ++ R.commentary x)
    printRatings' xs
