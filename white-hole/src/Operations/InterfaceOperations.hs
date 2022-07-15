{-# LANGUAGE OverloadedStrings #-}
module Operations.InterfaceOperations where

import Database.PostgreSQL.Simple
import Entities.User
import qualified Entities.Movie as M
import Entities.Serie
import Data.Char (toLower, toUpper, isNumber, isAlphaNum, isDigit)
import qualified Entities.Rating as R
import Operations.UserOperations
import Operations.MovieOperations
import Operations.MovieInterfaceOperations
import Operations.SerieInterfaceOperations
import Operations.UtilOperations
import System.Exit
import System.IO


signUp :: Connection -> IO()
signUp conn = do
    putStrLn ""
    putStrLn "Digite o seu nome:"
    userName <- getLine
    putStrLn "Digite o seu sobrenome:"
    userSurname <- getLine
    putStrLn "Digite o seu e-mail:"
    userEmail <- getLine
    putStrLn "Digite a sua senha:"
    userPassword <- getLine
    {- Chamada da função que cadastra o usuário no banco de dados! -}
    ok <- createUser conn $ User {email=userEmail, password=userPassword, nome=userName, sobrenome=userSurname}
    putStrLn (if ok then "Usuario cadastrado com sucesso!" else "Ja existe usuario cadastrado com esse e-mail!")


signIn :: Connection -> IO()
signIn conn = do
    putStrLn ""
    putStrLn "Digite o seu e-mail:"
    userEmail <- getLine
    putStrLn "Digite a sua senha:"
    userPassword <- getLine
    {- Chamada da função run que roda as opções de avaliação e de 
    pesquisa, assim como os rankings! -}
    authenticated <- authenticate conn userEmail userPassword
    if authenticated then do
        putStrLn ""
        putStrLn "Bem vindo!"
        user <- getUserByEmail conn userEmail
        run conn $ head user
    else do
        putStrLn ""
        putStrLn "E-mail ou senha inválidos!"
        


firstMenu :: Connection -> IO()
firstMenu conn = do
    putStrLn ""
    putStrLn "Bem vindo ao White Hole de filmes e séries!"
    putStrLn "Descubra novos filmes para assistir e avalie os que você já assistiu!"
    putStrLn ""
    putStrLn "1 - Logar no White Hole"
    putStrLn "2 - Cadastrar-se no White Hole"
    putStrLn "3 - Sair"
    putStrLn ""
    putStrLn "Digite a opção que deseja acessar:"
    option <- getLine
    if option == "1" then do
        signIn conn >> firstMenu conn
    else if option == "2" then do
        signUp conn >> firstMenu conn
    else if option == "3" then do
        putStrLn ""
        putStrLn "Obrigado, tenha um ótimo dia!"
        putStrLn ""
    else do
        putStrLn ""
        putStrLn "Digite uma opção válida na próxima!"
        putStrLn ""


run :: Connection -> User -> IO()
run conn user = do
    putStrLn ("Olá " ++ nome user ++ "!")
    putStrLn ""
    putStrLn "1 - Procurar por filme"
    putStrLn "2 - Minha lista de marcados para assistir depois"
    putStrLn "3 - 10 melhores filmes"
    putStrLn "4 - 10 melhores séries"
    putStrLn "5 - 10 melhores filmes por categoria"
    putStrLn "6 - 10 melhores séries por categoria"
    putStrLn "7 - Logout"
    putStrLn ""
    putStrLn "Digite a opção desejada: "
    hFlush stdout
    option <- getLine
    case option of
        "1" -> search conn user >> run conn user
        "2" -> myList conn user >> run conn user
        "3" -> tenBestMovies conn >> run conn user
        "4" -> tenBestSeries conn >> run conn user
        "5" -> tenBestMoviesByCategory conn >> run conn user
        "6" -> tenBestSeriesByCategory conn >> run conn user
        "7" -> putStrLn "" >> putStrLn "Até mais, volte sempre!"
        x -> putStrLn "Digite uma opção válida" >> run conn user


search :: Connection -> User -> IO()
search conn user = do
    putStrLn "---------------------- PESQUISAR ------------------------"
    putStrLn ""
    putStrLn "Digite o nome do filme ou série que deseja pesquisar (Se quiser voltar, aperte enter e digite 'v'):"
    putStrLn ""
    movie <- getLine
    putStrLn ""
    movies <- searchMovie conn (map toLower movie)
    if null movies then putStrLn "Não foi encontrado nenhum filme ou série com esse título!" else printMoviesList movies 1
    putStrLn ""

    putStrLn "Digite o número correspondente ao filme que quer ver ou as opções abaixo:"
    putStrLn "( p - Pesquisar novamente; v - Voltar ao menu principal; c - Cadastrar um novo filme ou série): "
    putStrLn ""

    option <- getLine
    if option == "v" then
        hFlush stdout
    else if option == "p" then
        search conn user
    else if option == "c" then
        registerMovieSerie conn user
    else if not (isANumber option True) || (((read option :: Int) > length movies) && (read option :: Int) < 1) then do
        putStrLn "Digite uma opção válida na próxima vez."
    else do
        showMovie conn user (movies !! ((read option :: Int) - 1)) >> search conn user


registerMovieSerie :: Connection -> User -> IO ()
registerMovieSerie conn user = do
    putStrLn ""
    putStrLn "1 - Adicionar filme"
    putStrLn "2 - Adicionar série"
    putStrLn "v - Voltar"
    putStrLn ""
    putStrLn "Digite a opção:"
    option <- getLine
    case option of
        "1" -> createMovie conn >> search conn user
        "2" -> createSerie conn >> search conn user
        "v" -> search conn user
        x -> putStrLn "Digite uma opção válida!" >> registerMovieSerie conn user


myList :: Connection -> User -> IO ()
myList conn user = do
    putStrLn "------ Minha Lista Para Assistir Depois ------"
    putStrLn ""
    movies <- getWatchLaterList conn user
    if null movies then do
        print "Lista vazia"
    else do
        printMoviesList movies 1
    putStrLn ""
    putStrLn "Opções:"
    putStrLn ""
    putStrLn "1 - Acessar filme"
    putStrLn "2 - Voltar"
    putStrLn ""
    putStrLn "Digite sua ação"
    option <- getLine
    if option == "1" then do
        putStrLn ""
        putStrLn "Qual filme você quer acessar? (Digite o número do filme da lista)"
        filme <- getLine
        if (read filme :: Int) > length movies || (read filme :: Int) <= 0 then do
            putStrLn "digite uma opção válida"
            myList conn user
        else do
            showMovie conn user (movies !! ((read filme :: Int) - 1)) >> myList conn user
    else do
        if option == "2" then do
            hFlush stdout
        else do
            putStrLn "Digite uma opção válida"
            myList conn user

