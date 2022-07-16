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
import Operations.SerieOperations
import qualified System.Process as System


signUp :: Connection -> IO()
signUp conn = do
    putStrLn ""
    putStrLn "Digite o seu nome:"
    userName <- getLine
    if null (strip userName) then do 
        putStrLn "Digite um nome válido" 
        clearScreenWithConfirmation
        return () 
    else do 
        putStrLn ""

    putStrLn "Digite o seu sobrenome:"
    userSurname <- getLine
    if null (strip userSurname) then do 
        putStrLn "Digite um sobrenome válido" 
        clearScreenWithConfirmation
        return ()
    else do 
        putStrLn ""

    putStrLn "Digite o seu e-mail:"
    userEmail <- getLine
    if null (strip userEmail) then do 
        putStrLn "Digite um e-mail válido" 
        clearScreenWithConfirmation
        return ()
    else do 
        putStrLn ""

    putStrLn "Digite a sua senha (tem que ter no mínimo 8 caracteres):"
    userPassword <- getLine
    if length (strip userPassword) < 9 then do 
        putStrLn "A senha não entende aos requisitos básicos!"
        clearScreenWithConfirmation
        return ()
    else do
        putStrLn ""
    ok <- createUser conn $ User {email=userEmail, password=userPassword, nome=userName, sobrenome=userSurname}
    putStrLn (if ok then "Usuario cadastrado com sucesso!" else "Ja existe usuario cadastrado com esse e-mail!")
    clearScreenWithConfirmation


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
        putStrLn "Credenciais válidas!"
        user <- getUserByEmail conn userEmail
        clearScreenWithConfirmation 
        run conn $ head user
    else do
        putStrLn ""
        putStrLn "E-mail ou senha inválidos!"
        clearScreenWithConfirmation
        


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
        clearScreenOnly >> signIn conn >> firstMenu conn
    else if option == "2" then do
        clearScreenOnly >> signUp conn >> firstMenu conn
    else if option == "3" then do
        putStrLn ""
        putStrLn "Obrigado, tenha um ótimo dia!"
        putStrLn ""
    else do
        putStrLn ""
        putStrLn "Digite uma opção válida na próxima!"
        putStrLn ""
        clearScreenWithConfirmation


run :: Connection -> User -> IO()
run conn user = do
    putStrLn ("Olá " ++ nome user ++ "!")
    putStrLn ""
    putStrLn "1 - Procurar por filme ou série"
    putStrLn "2 - Minha lista de marcados para assistir depois"
    putStrLn "3 - 10 melhores filmes"
    putStrLn "4 - 10 melhores séries"
    putStrLn "5 - 10 melhores filmes por categoria"
    putStrLn "6 - 10 melhores séries por categoria"
    putStrLn "7 - Recomendações para mim"
    putStrLn "8 - Logout"
    putStrLn ""
    putStrLn "Digite a opção desejada: "
    hFlush stdout
    option <- getLine
    clearScreenOnly
    case option of
        "1" -> clearScreenOnly >> search conn user >> run conn user
        "2" -> clearScreenOnly >> myList conn user >> run conn user
        "3" -> clearScreenOnly >> tenBestMovies conn user >> run conn user
        "4" -> clearScreenOnly >> tenBestSeries conn user >> run conn user
        "5" -> clearScreenOnly >> tenBestMoviesByCategory conn user >> run conn user
        "6" -> clearScreenOnly >> tenBestSeriesByCategory conn user >> run conn user
        "7" -> clearScreenOnly >> recomendations conn user >> run conn user
        "8" -> putStrLn "" >> putStrLn "Até mais, volte sempre!"
        x -> putStrLn "Digite uma opção válida" >> clearScreenWithConfirmation >> run conn user


search :: Connection -> User -> IO()
search conn user = do
    putStrLn "---------------------- PESQUISAR ------------------------"
    putStrLn ""
    putStrLn "Digite o nome do filme ou série que deseja pesquisar (Se "
    putStrLn "quiser voltar, aperte enter):"
    putStrLn ""
    title <- getLine
    if null (strip title) then do 
        clearScreenOnly
    else do
        putStrLn ""
        movies <- searchMovie conn (map toLower title)
        series <- searchSerie conn (map toLower title)
        if null movies && null series then do
            putStrLn "Não foi encontrado nenhum filme ou série com esse título!" 
        else do
            putStrLn "Filmes:"
            putStrLn ""
            printMoviesList movies 1
            putStrLn ""
            putStrLn "Séries:"
            putStrLn ""
            printSeriesList series (fromIntegral (length movies) + 1)

        putStrLn ""
        putStrLn "Digite o número correspondente ao filme ou a série que quer ver ou as opções abaixo:"
        putStrLn "( p - Pesquisar novamente; v - Voltar ao menu principal; c - Cadastrar um novo filme ou série): "
        putStrLn ""

        option <- getLine
        if option == "v" then
            clearScreenOnly
        else if option == "p" then
            clearScreenOnly >> search conn user
        else if option == "c" then
            clearScreenOnly >> registerMovieSerie conn user
        else if not (isANumber option True) || (((read option :: Int) > (length movies + length series)) || (read option :: Int) < 1) || null option then do
            putStrLn "Digite uma opção válida na próxima vez." >> clearScreenWithConfirmation
        else do
            if (read option :: Int) <= length movies then do
                clearScreenOnly >> showMovie conn user (movies !! ((read option :: Int) - 1)) >> search conn user
            else do
                clearScreenOnly >> showSerie conn user (series !! ((read option :: Int) - length movies - 1)) >> search conn user


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
        "1" -> clearScreenOnly >> createMovie conn >> search conn user
        "2" -> clearScreenOnly >> createSerie conn >> search conn user
        "v" -> clearScreenOnly >> search conn user
        x -> putStrLn "Digite uma opção válida!" >> clearScreenWithConfirmation >> registerMovieSerie conn user


myList :: Connection -> User -> IO ()
myList conn user = do
    putStrLn "------ Minha Lista Para Assistir Depois ------"
    putStrLn ""
    movies <- getWatchLaterList conn user
    series <- getWatchLaterListSeries conn user
    putStrLn "Filmes:"
    putStrLn ""
    if null movies then do
        print "Lista vazia"
    else do
        printMoviesList movies 1

    putStrLn ""
    putStrLn "Séries:"
    putStrLn ""
    
    if null series then do
        print "Lista vazia"
    else do
        printSeriesList series (fromIntegral (length movies) + 1)

    putStrLn ""
    putStrLn "Opções:"
    putStrLn ""
    putStrLn "1 - Acessar filme ou série"
    putStrLn "2 - Voltar"
    putStrLn ""
    putStrLn "Digite sua ação"
    option <- getLine
    if option == "1" then do
        putStrLn ""
        putStrLn "Qual filme ou série você quer acessar? (Digite o número do filme/série da lista; Para voltar, aperte Enter)"
        choice <- getLine
        if null choice then do 
            clearScreenOnly 
        else do 
            if (read choice :: Int) > length movies + length series || (read choice :: Int) <= 0 then do
                putStrLn ""
                putStrLn "digite uma opção válida" >> clearScreenWithConfirmation
                myList conn user
            else do
                if (read choice :: Int) <= length movies then do
                    clearScreenOnly >> showMovie conn user (movies !! ((read choice :: Int) - 1)) >> myList conn user
                else do
                    clearScreenOnly >> showSerie conn user (series !! ((read choice :: Int) - length movies - 1)) >> myList conn user

    else do
        if option == "2" then do
            clearScreenOnly
        else do
            putStrLn ""
            putStrLn "Digite uma opção válida" >> clearScreenWithConfirmation
            myList conn user


recomendations :: Connection -> User -> IO ()
recomendations conn user = do
    putStrLn ""
    putStrLn "--------------------------------------------------------"
    putStrLn "Recomendações"
    putStrLn "--------------------------------------------------------"
    putStrLn ""
    putStrLn "Aqui estão algumas recomendações de filmes e séries que "
    putStrLn "achamos que você vai gostar!"
    putStrLn ""
    putStrLn "Analizando o seu perfil... (aguarde)"
    putStrLn ""
    movies <- getRecomendationsOfMovies conn user
    series <- getRecomendationsOfSeries conn user
    putStrLn "Filmes:"
    putStrLn ""
    if null movies then do
        tenMovies <- getTenBestMovies conn 
        printMoviesList tenMovies 1 
    else do
        printMoviesList movies 1
    putStrLn ""
    putStrLn "Séries:"
    putStrLn ""
    if null series then do 
        tenSeries <- getTenBestSeries conn
        printSeriesList tenSeries (fromIntegral (length movies + 1))
    else do 
        printSeriesList series (fromIntegral (length movies + 1))
    putStrLn ""
    putStrLn "Qual filme ou série você quer acessar? (Digite o número do filme/série da lista; Se quiser voltar, aperte Enter)"
    choice <- getLine
    if null choice then do
        clearScreenOnly
    else do
        if (read choice :: Int) > length movies + length series || (read choice :: Int) <= 0 then do
            putStrLn ""
            putStrLn "digite uma opção válida" >> clearScreenWithConfirmation
            recomendations conn user
        else do
            if (read choice :: Int) <= length movies then do
                clearScreenOnly >> showMovie conn user (movies !! ((read choice :: Int) - 1)) >> recomendations conn user
            else do
                clearScreenOnly >> showSerie conn user (series !! ((read choice :: Int) - length movies - 1)) >> recomendations conn user