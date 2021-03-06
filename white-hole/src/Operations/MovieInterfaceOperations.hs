{-# LANGUAGE OverloadedStrings #-}
module Operations.MovieInterfaceOperations where

import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Movie as M
import Entities.Serie
import Data.Char (toLower, toUpper, isNumber, isAlphaNum, isDigit)
import qualified Entities.Rating as R
import Operations.UserOperations
import Operations.MovieOperations
import Operations.UtilOperations
import System.IO



createMovie :: Connection -> IO ()
createMovie conn = do
    putStrLn ""
    putStrLn "Qual o título do filme?"
    title <- getLine
    if null $ strip title then do
        putStrLn "O filme deve ter um título!"
        clearScreenWithConfirmation
    else do
        putStrLn "Qual a data de lançamento do filme? (yyyy-mm-dd)"
        releaseDate <- getLine
        if null (strip releaseDate) || length releaseDate /= 10 then do
            putStrLn "A data deve ser uma data válida!"
            clearScreenWithConfirmation
        else do 
            putStrLn "Qual a duração do filme? (Em minutos)"

            duration <- getLine
            if null (strip duration) || not (isANumber duration True) || length duration > 4 then do
                putStrLn "Digite uma duração válida"
                clearScreenWithConfirmation
            else do 
                putStrLn "Qual a sinopse do filme?"

                summaryT <- getLine
                putStrLn "Digite o nome dos diretores (separados por vírgula):"
                directors <- getLine
                putStrLn "Digite o nome dos atores (separados por vírgula):"
                actors <- getLine
                let summary = if null summaryT then "Sem sinopse!" else summaryT
                putStrLn ""
                putStrLn "Digite a quais categorias esse filme pertence (digite os números associados separados por espaços):"
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
                    putStrLn "É necessário que o filme tenha ao menos uma categoria!"
                    clearScreenWithConfirmation
                else do 
                    putStrLn "Quer realmente adicionar esse filme? (s/N)"

                    confirmation <- getLine
                    if null confirmation || map toLower confirmation == "n" then do
                        putStrLn ""
                        putStrLn "Ok!"
                        clearScreenWithConfirmation
                    else do
                        movie <- registerMovie conn title releaseDate duration summary
                        addCastingToMovie conn movie (splitItems actors [])
                        addDirectorsToMovie conn movie (splitItems directors [])
                        verified <- verifyCategories (words categoriesTemp)
                        let categories = if verified then convertCategories (words categoriesTemp) [] else []
                        addCategoriesToMovie conn movie categories
                        putStrLn ""
                        putStrLn "Filme cadastrado com sucesso!"
                        clearScreenWithConfirmation


printMoviesList :: [M.Movie] -> Integer -> IO ()
printMoviesList [] _ = return ()
printMoviesList movies seqNum = do
    putStrLn (show seqNum ++ " - " ++ capitalize (M.title (head movies)))
    printMoviesList (tail movies) (seqNum + 1)


tenBestMovies :: Connection -> User -> IO ()
tenBestMovies conn user = do
    putStrLn ""
    putStrLn "Os dez filmes mais bem avaliados no momento são: "
    putStrLn ""
    movies <- getTenBestMovies conn
    printMoviesList movies 1
    putStrLn ""
    putStrLn "Digite o número do filme para acessá-lo (aperte enter para voltar):"
    option <- getLine
    if null option then
        clearScreenOnly
    else if not (isANumber option True) || (((read option :: Int) > length movies) || (read option :: Int) < 1) then do
        putStrLn ""
        putStrLn "Digite uma opção válida na próxima vez."
        clearScreenWithConfirmation
    else do
        clearScreenOnly >> showMovie conn user (movies !! ((read option :: Int) - 1)) >> tenBestMovies conn user


tenBestMoviesByCategory :: Connection -> User -> IO ()
tenBestMoviesByCategory conn user = do
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
    if null option || not (isANumber option True) || (((read option :: Int) > 14) || (read option :: Int) < 1) then do
        putStrLn ""
        putStrLn "Digite uma opção válida na próxima vez."
        clearScreenWithConfirmation
    else do
        clearScreenOnly
        putStrLn "Os dez filmes mais bem avaliados dessa categoria no momento são (se a categoria não contiver filmes o suficiente, pode não aparecer dez filmes): "
        movies <- getMoviesByCategory conn $ head (convertCategories (words option) [])
        putStrLn ""
        printMoviesList movies 1
        putStrLn ""
        putStrLn "Digite o número do filme para acessá-lo (aperte enter para voltar):"
        option <- getLine
        if null option then
            clearScreenOnly
        else if not (isANumber option True) || (((read option :: Int) > length movies) || (read option :: Int) < 1) || null option then do
            putStrLn ""
            putStrLn "Digite uma opção válida na próxima vez."
            clearScreenWithConfirmation
        else do
            clearScreenOnly >> showMovie conn user (movies !! ((read option :: Int) - 1)) >> tenBestMoviesByCategory conn user


showMovie :: Connection -> User -> M.Movie -> IO ()
showMovie conn user movie = do
    putStrLn ""
    putStrLn "-------------------------------------------------"
    putStrLn ("        " ++ capitalize (M.title movie))
    putStrLn "-------------------------------------------------"
    putStrLn ""
    putStrLn ("Sinopse: " ++ M.summary movie)
    putStrLn ""
    putStrLn ("Duração (em minutos): " ++ M.duration movie)
    putStrLn ("Data de lançamento: " ++ M.releaseDate movie)
    categories <- getCategoriesOfMoviesInOneString conn movie
    putStrLn ("Categorias: " ++ capitalize categories)
    putStrLn ""
    putStrLn ("Nota geral: " ++ show (M.rating movie))
    putStrLn ""
    putStrLn "1 - Mostrar casting do filme."
    putStrLn "2 - Marcar como assistir depois."
    putStrLn "3 - Avaliar filme."
    putStrLn "4 - Mostrar avaliações do filme."
    putStrLn "5 - Voltar."
    putStrLn ""
    putStrLn "Digite a opção desejada: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> clearScreenOnly >> printCasting conn movie >> showMovie conn user movie
        "2" -> addToWatchLaterList conn user movie >> putStrLn "" >> putStrLn "Filme adicionado com sucesso na lista!" >> clearScreenWithConfirmation >> showMovie conn user movie
        "3" -> clearScreenOnly >> newRating conn user movie >> updateMovieRating conn movie >> showMovie conn user movie
        "4" -> clearScreenOnly >> printRatings conn movie >> showMovie conn user movie
        "5" -> clearScreenOnly
        x -> putStrLn "Digite uma opção válida" >> clearScreenWithConfirmation >> showMovie conn user movie



printCasting :: Connection -> M.Movie -> IO ()
printCasting conn movie = do
    casting <- getCasting conn movie
    if null casting then do
        putStrLn "" >> putStrLn "Não há um Casting cadastrado para esse filme!" >> clearScreenWithConfirmation
    else do
        clearScreenOnly
        putStrLn "-----------------------------------------------"
        putStrLn "                   Casting"
        putStrLn "-----------------------------------------------"
        putStrLn ""
        printCasting' casting
        clearScreenWithConfirmation


printCasting' :: [(String, String)] -> IO ()
printCasting' [] = return ()
printCasting' (x:xs) = do
    putStrLn ("* " ++ fst x ++ " - " ++ capitalize (snd x))
    printCasting' xs


newRating :: Connection -> User -> M.Movie -> IO ()
newRating conn user movie = do
    putStrLn ""
    putStrLn "Dê uma nota para o filme (De 1 a 5): "
    nota <- getLine
    if null nota || not (isANumber nota True && (read nota :: Int) `elem` [1..5]) then putStrLn "Digite um valor válido na próxima vez!" >> clearScreenWithConfirmation >> newRating conn user movie else putStrLn ""
    putStrLn "Faça um comentário sobre o filme (Se não quiser, basta apertar enter): "
    commentary <- getLine
    let comment = if null commentary then "Sem comentário" else commentary
    putStrLn ""
    putStrLn "Confirmar avaliação (s/N)"
    confirmation <- getLine
    if null confirmation || map toLower confirmation == "n" then do
        putStrLn "Ok!"
        clearScreenWithConfirmation
    else do
        confirmation <- avaluateMovie conn user movie (read nota :: Integer) commentary
        if confirmation then do
            clearScreenOnly
            putStrLn "Avaliação feita com sucesso!"
        else do
            clearScreenOnly
            putStrLn "Um mesmo usuário não pode avaliar o mesmo filme duas vezes!"
        clearScreenWithConfirmation


printRatings :: Connection -> M.Movie -> IO ()
printRatings conn movie = do
    ratings <- getRatings conn movie
    if null ratings then do
        putStrLn "" >> putStrLn "Esse filme ainda não foi avaliado por algum usuário!" >> clearScreenWithConfirmation
    else do
        clearScreenOnly
        putStrLn "-----------------------------------------------"
        putStrLn "                  Avaliações"
        putStrLn "-----------------------------------------------"
        putStrLn ""
        printRatings' ratings
        clearScreenWithConfirmation


printRatings' :: [R.Rating] -> IO ()
printRatings' [] = return ()
printRatings' (x:xs) = do
    putStrLn (R.userEmail x ++ " - " ++ show (R.rating x) ++ " - " ++ R.commentary x)
    printRatings' xs
