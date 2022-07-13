{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import Database.PostgreSQL.Simple
import Db.Operations
import Db.Configure
import Entities.User
import System.IO
import qualified Entities.Movie as M
import System.Exit
import Entities.Serie
import Data.Char (toLower, toUpper, isNumber, isAlphaNum, isDigit)

signUp :: Connection -> IO()
signUp conn = do
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
  putStrLn "Digite o seu e-mail:"
  userEmail <- getLine
  putStrLn "Digite a sua senha:"
  userPassword <- getLine
  {- Chamada da função run que roda as opções de avaliação e de 
  pesquisa, assim como os rankings! -}
  authenticated <- authenticate conn userEmail userPassword
  if authenticated then do
    putStrLn "Bem vindo!"
    user <- getUserByEmail conn userEmail
    run conn $ head user
  else do
    putStrLn "E-mail ou senha inválidos!"
    firstMenu conn


firstMenu :: Connection -> IO()
firstMenu conn = do
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
    signIn conn
  else if option == "2" then do
    signUp conn >> firstMenu conn
  else if option == "3" then do
    putStrLn "Obrigado, tenha um ótimo dia!"
    exitSuccess
  else do
    putStrLn "Digite uma opção válida na próxima!"
    putStrLn ""


run :: Connection -> User -> IO()
run conn user = do
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
    "1" -> search conn user
    "2" -> myList conn user
    "3" -> tenBestMovies conn
    "4" -> tenBestSeries conn
    "5" -> tenBestMoviesByCategory conn
    "6" -> tenBestSeriesByCategory conn
    "7" -> firstMenu conn
    x -> putStrLn "Digite uma opção válida" >> run conn user

isANumber :: String -> Bool -> Bool
isANumber [] _ = True
isANumber (x:xs) True = isDigit x && isANumber xs True


search :: Connection -> User -> IO()
search conn user = do
  putStrLn "----------PESQUISAR----------"
  putStrLn ""
  putStrLn "Digite o nome do filme que deseja pesquisar"
  putStrLn ""
  movie <- getLine
  putStrLn ""
  movies <- searchMovie conn (map toLower movie)
  printMoviesList movies 1
  putStrLn ""

  putStrLn "Digite o número correspondente ao filme que quer ver (0 - Pesquisar novamente, -1 - Voltar ao menu principal): "
  putStrLn ""

  option <- getLine
  if option == "-1" then
    run conn user
  else if option == "0" then
    search conn user
  else if isANumber option True || (read option :: Int) > length movies then do
    putStrLn "Digite uma opção válida na próxima vez."
  else do
    -- showMovie option 
    putStrLn "-- showMovie option"


{-Funções criadas apenas para testar a função run-}
myList :: Connection -> User -> IO ()
myList conn user = do
  putStrLn "------ Minha Lista Para Assistir Depois ------"
  putStrLn ""
  movies <- getWatchLaterList conn user
  if null movies then do
    print "Lista vazia"
    run conn user
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
      showMovie conn (movies !! (read filme :: Int))
  else do
    if option == "2" then do
      run conn user
    else do
      putStrLn "Digite uma opção válida"
      myList conn user



printMoviesList :: [M.Movie] -> Integer -> IO ()
printMoviesList [] _ = return ()
printMoviesList movies seqNum = do
  putStrLn (show seqNum ++ " - " ++ capitalize (M.title (head movies)))
  printMoviesList (tail movies) (seqNum + 1)


capitalize :: String -> String
capitalize name = toUpper (head name) : tail name


tenBestMovies :: Connection -> IO ()
tenBestMovies conn = undefined


tenBestSeries :: Connection -> IO ()
tenBestSeries conn = undefined


tenBestMoviesByCategory :: Connection -> IO ()
tenBestMoviesByCategory conn = undefined


tenBestSeriesByCategory :: Connection -> IO ()
tenBestSeriesByCategory conn = undefined


showMovie :: Connection -> M.Movie -> IO ()
showMovie conn movie = undefined

showSerie :: Connection -> Serie -> IO()
showSerie conn serie = undefined

main :: IO ()
main = do
  conn <- connectToDB
  firstMenu conn





