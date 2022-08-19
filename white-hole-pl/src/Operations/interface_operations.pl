:- module(interfaceop, [first_menu/1]).
:- use_module('./util.pl', [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).

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
    clear_screen_with_confirmation.
    authenticate(Connection, UserEmail, UserPassword, Autenticated),
    (Autenticated = "1" ->
        getUserByEmail(Connection, UserEmail, User),
        clear_screen, run(Connection, User);
    (Autenticated = "0"->
        writeln(''),
        writeln('E-mail ou senha inválidos!');
        !
    )
    ).



signup(Connection) :-
    writeln(''),
    get_input('Digite o seu nome:', userName),
    writeln(''),
    check_valid_value(userName, Connection),

    get_input('Digite seu sobrenome:', userSurname),
    check_valid_value(userSurname, Connection),
    writeln(''),

    get_input('Digite um e-mail', userEmail),
    check_valid_value(userEmail, Connection),
    writeln(''),

    get_input('Digite a sua senha (tem que ter no mínimo 8 caracteres):', userPassword),
    check_valid_value(userPassword, Connection),
    writeln(''),

    /* cahamda da função que envia os dados. */


    clear_screen_with_confirmation,
    first_menu(Connection).

check_valid_value("", Connection):- writeln('Digite valores validos na próxima!'), writeln(''), clear_screen_with_confirmation, signup(Connection).