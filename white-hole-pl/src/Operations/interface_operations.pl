:- module(interfaceop, [first_menu/1]).
:- use_module('./util.pl', [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).
:- use_module('./user_operations.pl').
:- use_module('./movie_operations.pl').
:- use_module('./serie_operations.pl').
:- use_module('./serie_interface_operations.pl').
:- use_module('./movie_interface_operations.pl').


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
        getUserByEmail(Connection, UserEmail, [User]),
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
    createUser(Connection, UserEmail, UserPassword, UserName, UserSurName, Confirmacao),
    (Confirmacao =:= 1 ->
         writeln('Usuario cadastrado com sucesso!'),
         clear_screen_with_confirmation,
         first_menu(Connection);
         writeln('Usuario não cadastrado!'),
         clear_screen_with_confirmation,
         first_menu(Connection)
    ).


run(Connection, User):-
    writeln('Olá!'),
    writeln(''),
    writeln('1 - Procurar por filme ou série'),
    writeln('2 - Minha lista de marcados para assistir depois'),
    writeln('3 - 10 melhores filmes'),
    writeln('4 - 10 melhores séries'),
    writeln('5 - 10 melhores filmes por categoria'),
    writeln('6 - 10 melhores séries por categoria'),
    writeln('7 - Recomendações para mim'),
    writeln('8 - Logout'),
    writeln(''),
    get_input('Digite a opção desejada: ', Option),
    (
        Option = "1" -> clear_screen, search(Connection, User), run(Connection, User);
        Option = "2" -> clear_screen, myList(Connection, User), run(Connection, User);
        Option = "3" -> clear_screen, tenBestMovies(Connection, User), run(Connection, User);
        Option = "4" -> clear_screen, serie_interface_operations:tenBestSeries(Connection, User), run(Connection, User);
        Option = "5" -> clear_screen, tenBestMoviesByCategory(Connection, User), run(Connection, User);
        Option = "6" -> clear_screen, serie_interface_operations:tenBestSeriesByCategory(Connection, User), run(Connection, User);
        Option = "7" -> clear_screen, recomendations(Connection, User), run(Connection, User);
        Option = "8" -> writeln(""), writeln("Até mais, volte sempre!");
            writeln("Digite uma opção válida"), clear_screen_with_confirmation, run(Connection, User)
    ).


search(Connection, User):-
    writeln('---------------------- PESQUISAR ------------------------'),
    writeln(""),
    writeln("Digite o nome do filme ou série que deseja pesquisar (Se "),
    writeln("quiser voltar, aperte enter):"),
    get_input("", Title),
    util:strip(Title, TitleTrimmed),
    (TitleTrimmed = "" -> clear_screen ;
        writeln(""),
        string_lower(TitleTrimmed, TitleForSearch),
        searchMovie(Connection, TitleForSearch, Movies),
        searchSerie(Connection, TitleForSearch, Series),
        ( Movies = [], Series = [] -> 
            writeln("Não foi encontrado nenhum filme ou série com esse título!" );
            writeln("Filmes:"),
            writeln(""),
            printMoviesList(Movies, 1),
            writeln(""),
            writeln("Séries:"),
            writeln(""),
            length(Movies, MLength),
            SeriesNum is MLength + 1,
            printSeriesList(Series, SeriesNum)
            ),
        writeln(""),
        writeln("Digite o número correspondente ao filme ou a série que quer ver ou as opções abaixo:"),
        writeln("( p - Pesquisar novamente; v - Voltar ao menu principal; c - Cadastrar um novo filme ou série): "),
        get_input("", Option),
        ( Option = "v" -> clear_screen;
            Option = "p" -> clear_screen, search(Connection, User);
            Option = "c" -> clear_screen, registerMovieSerie(Connection, User);
            length(Movies, MLength), length(Series, SLength), Total is MLength + SLength,
            util:isANumber(NOption, Option), NOption =< Total, NOption > 0, Option \= "" ->
                ( NOption =< MLength -> 
                    nth1(NOption, Movies, Movie),
                    clear_screen, showMovie(Connection, User, Movie),search(Connection, User)
                    ;
                    SIndex is NOption - MLength,
                    nth1(SIndex, Series, Serie),
                    clear_screen, showSerie(Connection, User, Serie),search(Connection, User)
                )
                ;
                writeln("Digite uma opção válida na próxima vez."), clear_screen_with_confirmation    
            )
        ).


registerMovieSerie(Connection, User):-
    writeln(""),
    writeln("1 - Adicionar filme"),
    writeln("2 - Adicionar série"),
    writeln("v - Voltar"),
    writeln(""),
    get_input("Digite a opção:", Option),
    (
        Option = "1" -> clear_screen, createMovie(Connection), search(Connection, User);
        Option = "2" -> clear_screen, createSerie(Connection), search(Connection, User);
        Option = "v" -> clear_screen, search(Connection, User);
            writeln("Digite uma opção válida!"), clear_screen_with_confirmation, registerMovieSerie(Connection, User)
    ).


myList(Connection, User):-
    writeln("------ Minha Lista Para Assistir Depois ------"),
    writeln(""),
    movie_operations:getWatchLaterList(Connection, User, Movies),
    serie_operations:getWatchLaterListSeries(Connection, User, Series),
    writeln("Filmes:"),
    writeln(""),
    (Movies = [] -> writeln("Lista vazia"); printMoviesList(Movies, 1)),
    
    writeln(""),
    writeln("Séries:"),
    writeln(""),
    SL is MLength + 1,
    (Series = [] -> writeln("Lista vazia"); length(Movies, MLength), printSeriesList(Movies, SL)),

    writeln(""),
    writeln("Opções:"),
    writeln(""),
    writeln("1 - Acessar filme ou série"),
    writeln("2 - Voltar"),
    writeln(""),
    get_input("Digite sua ação", Option),
    ( Option = "1" ->
        writeln(""),
        get_input("Qual filme ou série você quer acessar? (Digite o número do filme/série da lista; Para voltar, aperte Enter)", Choice),
        ( Choice = "" -> 
            clear_screen;
            length(Movies, MLength), length(Series, SLength),
            ( util:isANumber(NChoice, Choice), NChoice < MLength + SLength ->
                ( NChoice > 1 -> 
                    ( NChoice =< MLength ->
                        nth1(NChoice, Movies, Movie),
                        clear_screen, 
                        showMovie(Connection, User, Movie), myList(Connection, User);
                        nth1(NChoice - MLength, Series, Serie),
                        clear_screen,
                        showSerie(Connection, User, Serie), myList(Connection, User)
                    )
                    ;
                    writeln(""),
                    writeln("digite uma opção válida"), clear_screen_with_confirmation,
                    myList(Connection, User)
                );
                writeln(""),
                writeln("digite uma opção válida"), clear_screen_with_confirmation,
                myList(Connection, User)
            )
        );
    Option = "2" -> clear_screen;
        writeln(""),
        writeln("Digite uma opção válida"), clear_screen_with_confirmation,
        myList(Connection, User)
    ).

recomendations(Connection, User):-
    writeln(""),
    writeln("--------------------------------------------------------"),
    writeln("Recomendações"),
    writeln("--------------------------------------------------------"),
    writeln(""),
    writeln("Aqui estão algumas recomendações de filmes e séries que "),
    writeln("achamos que você vai gostar!"),
    writeln(""),
    writeln("Analizando o seu perfil... (aguarde)"),
    writeln(""),
    movie_operations:getRecomendationsOfMovies(Connection, User, Movies),
    serie_operations:getRecomendationsOfSeries(Connection, User, Series),
    writeln("Filmes:"),
    writeln(""),
    getTenBestMovies(Connection, TenBestMovies),
    ( Movies = [] -> length(TenBestMovies, MLength), printMoviesList(TenBestMovies, 1); length(Movies, MLength), printMoviesList(Movies, 1) ),
    writeln(""),
    writeln("Séries:"),
    writeln(""),
    getTenBestSeries(Connection, TenBestSeries),
    ( Series = [] -> length(TenBestSeries, SLength), printMoviesList(TenBestSeries, 1); length(Series, SLength), MSL is MLength + 1, printSeriesList(Series, MSL) ),
    writeln(""),
    get_input("Qual filme ou série você quer acessar? (Digite o número do filme/série da lista; Se quiser voltar, aperte Enter)", Choice),
    ( Choice = "" -> 
            clear_screen;
            length(Movies, MLength), length(Series, SLength),
            ( util:isANumber(NChoice, Choice), NChoice < MLength + SLength ->
                ( NChoice > 1 -> 
                    ( NChoice =< MLength ->
                        nth1(NChoice, Movies, Movie),
                        clear_screen, 
                        showMovie(Connection, User, Movie), recomendations(Connection, User);
                        nth1(NChoice - MLength, Series, Serie),
                        clear_screen,
                        showSerie(Connection, User, Serie), recomendations(Connection, User)
                    )
                    ;
                    writeln(""),
                    writeln("digite uma opção válida"), clear_screen_with_confirmation,
                    recomendations(Connection, User)
                );
                writeln(""),
                writeln("digite uma opção válida"), clear_screen_with_confirmation,
                recomendations(Connection, User)
            )
    ).