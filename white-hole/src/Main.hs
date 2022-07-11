{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.PostgreSQL.Simple
import Db.Operations
import Db.Configure
import Entities.User
import System.IO

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
  let answer = if authenticated then "Bem vindo!" else "E-mail ou senha inválidos!"
  putStrLn answer


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
  if option == "1" then
    signIn conn
  else if option == "2" then
    signUp conn >> firstMenu conn
  else if option == "3" then
    putStrLn "Obrigado, tenha um ótimo dia!"
  else do
    putStrLn "Digite uma opção válida na próxima!"
    putStrLn ""
    {-conn <- connectionMyDB
    [Only result] <- query_ conn "select 2 + 2" :: IO [Only Int]
    putStrLn $ show result-}


data RunOption
  = Seach
  | MyList
  | TenBestMovies
  | TenBestSeries
  | TenBestMoviesByCategory
  | TenBestSeriesByCategory
  | FirstMenu
  deriving Show


run :: IO RunOption
run = do
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
    "1" -> return Seach
    "2" -> return MyList
    "3" -> return TenBestMovies
    "4" -> return TenBestSeries
    "5" -> return TenBestMoviesByCategory
    "6" -> return TenBestSeriesByCategory
    "7" -> return FirstMenu
  
{-Funções criadas apenas para testar a função run-}
seach :: IO ()
seach = undefined

myList :: IO ()
myList = undefined

tenBestMovies :: IO ()
tenBestMovies = undefined

tenBestSeries :: IO ()
tenBestSeries = undefined

tenBestMoviesByCategory :: IO ()
tenBestMoviesByCategory = undefined

tenBestSeriesByCategory :: IO ()
tenBestSeriesByCategory = undefined

main :: IO ()
main = do
  conn <- connectToDB
  firstMenu conn
  option <- run
  case option of
    Seach                   -> seach
    MyList                  -> myList
    TenBestMovies           -> tenBestMovies
    TenBestSeries           -> tenBestSeries
    TenBestMoviesByCategory -> tenBestMoviesByCategory
    TenBestSeriesByCategory -> tenBestSeriesByCategory
    FirstMenu               -> firstMenu conn
      
