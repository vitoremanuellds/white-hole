:- module(interfaceop, [first_menu/1]).
:- use_module('./util.pl', [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).
:- use_module('./user_operations.pl').

first_menu(Connection) :-
    writeln(''),
    writeln('Bem vindo ao White Hole de filmes e séries!'),
    writeln('Descubra novos filmes para assistir e avalie os que você já assistiu!'),
    writeln(''),
    writeln('1 - Logar no White Hole'),
    writeln('2 - Cadastrar-se no White Hole'),
    writeln('3 - Sair'),
    writeln(''),
    get_input('Digite a opção que deseja acessar:', Option),
    (Option = "1" -> 
        clear_screen, signin(Connection), first_menu(Connection);
    (Option = "2" ->
        clear_screen, signup(Connection), first_menu(Connection);
    (Option = "3" ->
        clear_screen, writeln('Obrigado, tenha um ótimo dia!'), clear_screen_with_confirmation, halt;
    (Option = _ ->
        clear_screen, writeln('Digite uma opção válida na próxima!'), writeln(''), clear_screen_with_confirmation, first_menu(Connection);
        !
    )
    )    
    )
    ).


signin(Connection) :-
    writeln(''),
    get_input('Digite o seu e-mail:', UserEmail),
    writeln(''),
    get_input('Digite a sua senha:', UserPassword),
    clear_screen,
    authenticate(Connection, UserEmail, UserPassword, Autenticated),
    (Autenticated =:= 1 ->
        getUserByEmail(Connection, UserEmail, User),
        run(Connection, User);
        writeln(''),
        writeln('E-mail ou senha inválidos!'),
        clear_screen_with_confirmation
        
    ).

signup(Connection) :-
    writeln(''),
    get_input('Digite o seu nome:', UserName),
    (UserName = "" ->
         writeln('Digite um nome valido'),
         signup(Connection);
         !
    ),
    writeln(''),
    get_input('Digite o seu sobrenome:', UserSurName),
    (UserSurName = "" ->
         writeln('Digite um sobrenome valido'),
         signup(Connection);
         !
    ), 
    writeln(''),
    get_input('Digite um E-mail:', UserEmail),
    (UserEmail = "" ->
         writeln('Digite um E-mail valido'),
         signup(Connection);
         !
    ),
    writeln(''),
    get_input('Digite a sua senha (tem que ter no mínimo 8 caracteres):', UserPassword),
    (UserPassword = "" ->
         writeln('Digite uma senha valida'),
         signup(Connection);
         !
    ), 

    createUser(Connection, UserEmail, UserPassword, UserName, UserSurName, Confirmacao)
    ,

    (Confirmacao =:= 1 ->
         writeln('Usuario cadastrado com sucesso!'),
         clear_screen_with_confirmation,
         first_menu(Connection);
         writeln('Usuario não cadastrado!'),
         clear_screen_with_confirmation,
         first_menu(Connection)
    ).




    


run(Connection, User) :-
    writeln('1 - Procurar por filme ou série'),
    writeln('2 - Minha lista de marcados para assistir depois'),
    writeln('3 - 10 melhores filmes'),
    writeln('4 - 10 melhores séries'),
    writeln('5 - 10 melhores filmes por categoria'),
    writeln('6 - 10 melhores séries por categoria'),
    writeln('7 - Recomendações para mim'),
    writeln('8 - Logout'),
    writeln(''),
    get_input('Digite a opção desejada: ', Opc),
    (Opc = "1" ->
        clear_screen, search(Connection, User), run(Connection, User);
        !
    ).

search(Connection, User) :-
    writeln('---------------------- PESQUISAR ------------------------').
    



