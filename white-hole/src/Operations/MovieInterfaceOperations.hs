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
    if null title then
        putStrLn "O filme deve ter um título!" >> createMovie conn
    else
        putStrLn "Qual a data de lançamento do filme? (yyyy-mm-dd)"

    releaseDate <- getLine
    putStrLn "Qual a duração do filme? (Em minutos)"
    duration <- getLine
    putStrLn "Qual a sinopse do filme?"
    summaryT <- getLine
    putStrLn "Digite o nome dos diretores (separados por vírgula):"
    directors <- getLine
    putStrLn "Digite o nome dos atores (separados por vírgula):"
    actors <- getLine
    let summary = if summaryT == "" then "No comments!" else summaryT

    putStrLn "Digite a quais categorias esse filme pertence (digite os números associados separados por espaços):"
    putStrLn "1 - Ação          8  - Fantasia"
    putStrLn "2 - Suspense      9  - Documentário"
    putStrLn "3 - Romance       10 - Drama"
    putStrLn "4 - Comédia       11 - Anime"
    putStrLn "5 - Terror        12 - Mistério"
    putStrLn "6 - Aventura      13 - Infantil"
    putStrLn "7 - Investigação  14 - Ficção científica"
    putStrLn ""

    categoriesTemp <- getLine
    putStrLn "Quer realmente adicionar esse filme? (s/N)"
    confirmation <- getLine
    if null confirmation || map toLower confirmation == "n" then do
        putStrLn "Ok!"
    else do
        movie <- registerMovie conn title releaseDate duration summary
        addCastingToMovie conn movie (splitItems actors [])
        addDirectorsToMovie conn movie (splitItems directors [])
        verified <- verifyCategories (words categoriesTemp)
        let categories = if verified then convertCategories (words categoriesTemp) [] else []
        addCategoriesToMovie conn movie categories
        putStrLn ""
        putStrLn "Filme cadastrado com sucesso!"


printMoviesList :: [M.Movie] -> Integer -> IO ()
printMoviesList [] _ = return ()
printMoviesList movies seqNum = do
    putStrLn (show seqNum ++ " - " ++ capitalize (M.title (head movies)))
    printMoviesList (tail movies) (seqNum + 1)


tenBestMovies :: Connection -> IO ()
tenBestMovies conn = undefined


tenBestMoviesByCategory :: Connection -> IO ()
tenBestMoviesByCategory conn = undefined


showMovie :: Connection -> User -> M.Movie -> IO ()
showMovie conn user movie = do
    putStrLn "-------------------------------------------------"
    putStrLn (M.title movie)
    putStrLn "-------------------------------------------------"
    putStrLn ("Duração (em minutos): " ++ M.summary movie)
    putStrLn ("Data de lançamento: " ++ M.releaseDate movie)
    categories <- getCategoriesOfMoviesInOneString conn movie
    putStrLn ("Categorias: " ++ categories)
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
        "1" -> printCasting conn movie >> showMovie conn user movie
        "2" -> addToWatchLaterList conn user movie >> putStrLn "Filme adicionado com sucesso na lista!" >> showMovie conn user movie
        "3" -> newRating conn user movie >> showMovie conn user movie
        "4" -> printRatings conn movie >> showMovie conn user movie
        "5" -> hFlush stdout
        x -> putStrLn "Digite uma opção válida" >> showMovie conn user movie



printCasting :: Connection -> M.Movie -> IO ()
printCasting conn movie = do
    casting <- getCasting conn movie
    if null casting then putStrLn "Não há um Casting cadastrado para esse filme!" else printCasting' casting


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
    if not (isANumber nota True) then putStrLn "Digite um valor válido na próxima vez!" >> newRating conn user movie else putStrLn ""
    putStrLn "Faça um comentário sobre o filme (Se não quiser, basta apertar enter): "
    commentary <- getLine
    putStrLn ""
    avaluateMovie conn user movie (read nota :: Integer) commentary
    putStrLn "Avaliação feita com sucesso!"

printRatings :: Connection -> M.Movie -> IO ()
printRatings conn movie = do
    ratings <- getRatings conn movie
    if null ratings then putStrLn "Esse filme ainda não foi avaliado por algum usuário!" else printRatings' ratings


printRatings' :: [R.Rating] -> IO ()
printRatings' [] = return ()
printRatings' (x:xs) = do
    putStrLn (R.userEmail x ++ " - " ++ show (R.rating x) ++ " - " ++ R.commentary x)
    printRatings' xs
