module Main where

signUp :: IO()
signUp = do
  putStrLn "Digite o seu nome:"
  nome <- getLine
  putStrLn "Digite o seu sobrenome:"
  sobrenome <- getLine
  putStrLn "Digite o seu e-mail:"
  email <- getLine
  putStrLn "Digite a sua senha:"
  password <- getLine
  {- Chamada da função que cadastra o usuário no banco de dados! -}
  print "Usuario cadastrado com sucesso!"


signIn :: IO()
signIn = do
  putStrLn "Digite o seu e-mail:"
  email <- getLine
  putStrLn "Digte a sua senha:"
  password <- getLine
  {- Chamada da função run que roda as opções de avaliação e de 
  pesquisa, assim como os rankings! -}
  print "Bem vindo!"


main :: IO ()
main = do
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
    signIn
  else if option == "2" then
    signUp
  else if option == "3" then
    putStrLn "Obrigado, tenha um ótimo dia!"
  else do
    putStrLn "Digite uma opção válida na próxima!"
    putStrLn ""
    main