:- module(movie_interface_operations, [
        createMovie/1,
        printMoviesList/2,
        tenBestMovies/2,
        tenBestMoviesByCategory/2,
        showMovie/3,
        printCasting/2,
        printCastingAux/1,
        newRating/3,
        printRatings/2,
        printRatingsAux/1
    ]).
:- use_module('./util.pl').
:- use_module('../Db/dbOperations.pl').
:- use_module('./user_operations.pl').
:- use_module('./movie_operations.pl').

createMovie(Connection):-
    writeln(""),
    get_input("Qual o título do filme?", X),
    util:strip(X, Title),
    (Title = "" -> writeln("O filme deve ter um título!"), 
        clear_screen_with_confirmation;
        get_input("Qual a data de lançamento do filme? (yyyy-mm-dd)", Y),
        util:strip(Y, ReleaseDate),
        (ReleaseDate = "" -> writeln("A data deve ser uma data válida!"),
            clear_screen_with_confirmation;
            get_input("Qual a duração do filme? (Em minutos)", Z),
            util:strip(Z, Movies),
            ( Movies \= "", util:isANumber(NMovies, Movies), NMovies < 10000 ->
                get_input("Qual a sinopse do filme?", Sinopse),
                get_input("Digite o nome dos diretores (separados por vírgula):", Directors),
                get_input("Digite o nome dos atores (separados por vírgula):", Actors),
                Summary,
                ( Sinopse = "" -> Summary = "Sem sinopse"; Summary = Sinopse),
                writeln(""),
                writeln("Digite a quais categorias esse filme pertence (digite os números associados separados por espaços):"),
                writeln(""),
                writeln("1 - Ação          8  - Fantasia"),
                writeln("2 - Suspense      9  - Documentário"),
                writeln("3 - Romance       10 - Drama"),
                writeln("4 - Comédia       11 - Anime"),
                writeln("5 - Terror        12 - Mistério"),
                writeln("6 - Aventura      13 - Infantil"),
                writeln("7 - Investigação  14 - Ficção científica"),
                get_input("", A),
                util:strip(A, Categories),
                util:split_string(Categories, " ", " ", SplitedCategories),
                (Categories \= "", util:verify_cartegory(SplitedCategories) -> 
                    get_input("Quer realmente adicionar esse filme? (s/N)", Confirmation),
                    string_lower(Confirmation, LowerCase),
                    ( Confirmation \= "", LowerCase \= "n" -> 
                        writeln(""),
                        writeln("Ok!"),
                        clear_screen_with_confirmation;
                        registerMovie(Connection, Title, ReleaseDate, Duration, Summary, Movie),
                        util:splitItems(Actors, ActorsAdd),
                        addCastingToMovie(Connection, Movie, ActorsAdd),
                        util:splitItems(Directors, DirectorsAdd),
                        addDirectorsToMovie(Connection, Movie, DirectorsAdd),
                        util:convertCategories(SplitedCategories, [], CategoriesConverted),
                        addCategoriesToMovie(Connection, Movie, CategoriesConverted),
                        writeln(""),
                        writeln("Filme cadastrado com sucesso!"),
                        clear_screen_with_confirmation
                    )    
                    ;
                    writeln("É necessário que o filme tenha ao menos uma categoria!"),
                    clear_screen_with_confirmation
                )
                ;
                writeln("Digite uma duração válida"),
                clear_screen_with_confirmation
            )
        ),
    ).

printMoviesList([], _).
printMoviesList([Movie | T], Num):-
    Movie = row(_, Title, _, _, _, _),
    util:capitalize(Title, CapitalizedTitle),
    swritef(Saida, "%w  - %w", [Num, CapitalizedTitle]),
    writeln(Saida),
    NextNum is Num + 1,
    printMoviesList(T, NextNum).

tenBestMovies(Connection, User):-
    writeln(""),
    writeln("Os dez filmes mais bem avaliados no momento são: "),
    writeln(""),
    getTenBestMovies(Connection, Movies),
    printMoviesList(Movies, 1),
    writeln(""),
    get_input("Digite o número do filme para acessá-lo (aperte enter para voltar):", Option),
    length(Movies, SLength),
    (Option = "" -> clear_screen;
        ( util:isANumber(NOption, Option), NOption < SLength + 1, NOption > 0 ->
            clear_screen,
            nth1(NOption, Movies, Movie),
            showMovie(Connection, User, Movie),
            tenBestMovies(Connection, User);
            writeln(""),
            writeln("Digite uma opção válida na próxima vez."),
            clear_screen_with_confirmation
        )  
    ).

tenBestMoviesByCategory(Connection, User):-
    writeln(""),
    writeln("Selecione a categoria que você quer vizualizar:"),
    writeln(""),
    writeln("1 - Ação          8  - Fantasia"),
    writeln("2 - Suspense      9  - Documentário"),
    writeln("3 - Romance       10 - Drama"),
    writeln("4 - Comédia       11 - Anime"),
    writeln("5 - Terror        12 - Mistério"),
    writeln("6 - Aventura      13 - Infantil"),
    writeln("7 - Investigação  14 - Ficção científica"),
    writeln(""),
    writeln("Digite o número da categoria:"),
    get_input("", Option),
    util:strip(Option, OptionTrimmed),
    ( OptionTrimmed \= "", util:isANumber(NOption, OptionTrimmed), NOption < 15, NOption > 0 ->
        clear_screen,
        writeln("Os dez filmes mais bem avaliados dessa categoria no momento são (se a categoria não contiver filmes o suficiente, pode não aparecer dez filmes): "),
        util:convertCategories([NOption], [], Categories),
        getMoviesByCategory(Connection, Categories, Movies),
        writeln(""),
        printMoviesList(Movies, 1),
        writeln(""),
        get_input("Digite o número do filme para acessá-lo (aperte enter para voltar):", Input),
        util:strip(Input, InputTrimmed),

        ( InputTrimmed = "" -> clear_screen;
            InputTrimmed \= "", util:isANumber(NInput, InputTrimmed), NInput < 15, NInput > 0 ->
            clear_screen,
            nth1(NInput, Movies, Movie),
            showMovie(Connection, User, Movie),
            tenBestMoviesByCategory(Connection, User)
        );
        writeln(""),
        writeln("Digite uma opção válida na próxima vez."),
        clear_screen_with_confirmation
    ).

showMovie(Connection, User, Movie):-
    Movie = row(_, Title, RDate, Duration, Summary, Rating),
    util:capitalize(Title, CTitle),
    writeln("-------------------------------------------------"),
    writeln(CTitle),
    writeln("-------------------------------------------------"),
    writeln(""),
    swritef(Sinopse, "Sinopse: %w", [Summary]),
    writeln(Sinopse),
    writeln(""),
    write("Duração (em minutos): "), writeln(Duration),
    writeln("Data de lançamento: "), writeln(RDate),
    getCategoriesOfMoviesInOneString(Connection, Movie, CategoriesString),
    write("Categorias: "), writeln(CategoriesString),
    writeln(""),
    write("Nota geral: "), writeln(Rating),
    writeln(""),
    writeln("1 - Mostrar casting do filme."),
    writeln("2 - Marcar como assistir depois."),
    writeln("3 - Avaliar filme."),
    writeln("4 - Mostrar avaliações do filme."),
    writeln("5 - Voltar."),
    writeln(""),
    get_input("Digite a opção desejada: ", Option),
    ( Option = "1" -> clear_screen, printCasting(Connection, Movie), showMovie(Connection, User, Movie);
    Option = "2" -> clear_screen, movie_operations:addToWatchLaterList(Connection, User, Movie, _), writeln("Filme adicionado com sucesso na lista!"), clear_screen_with_confirmation, showMovie(Connection, User, Movie);
    Option = "3" -> clear_screen, newRating(Connection, User, Movie), movie_operations:updateMovieRating(Connection, Movie, _), showMovie(Connection, User, Movie);
    Option = "4" -> clear_screen, printRatings(Connection, Movie), showMovie(Connection, User, Movie);
    Option = "5" -> clear_screen;
        writeln("Digite uma opção válida"), clear_screen_with_confirmation, showMovie(Connection, User, Movie)
    ).

printCasting(Connection, Movie):-
    getCasting(Connection, Movie, Casting),
    ( Casting = [] -> writeln(""), writeln("Não há um Casting cadastrado para esse filme!"), clear_screen_with_confirmation;
    clear_screen,
    writeln("------------------------------------------------"),
    writeln("                    Casting"),
    writeln("------------------------------------------------"),
    writeln(""),
    printCastingAux(Casting), clear_screen_with_confirmation
    ).

printCastingAux([]).
printCastingAux([ row(X, Y) | T]):-
    util:capitalize(Y, Name),
    write("* "), write(X), write(" - "), writeln(Name),
    printCastingAux(T).

newRating(Connection, User, Movie):-
    writeln(""),
    get_input("Dê uma nota para o filme (De 1 a 5): ", Rating),
    ( util:isANumber(N, Rating), member(N, [1,2,3,4,5]) ->
        writeln(""),
        get_input("Faça um comentário sobre o filme (Se não quiser, basta apertar enter): ", C),
        ( C = "" -> Comment = "Sem comentario" ;
            Comment = C
        ),
        writeln(""),
        get_input("Confirmar avaliação (s/N)", Confirmation),
        string_lower(Confirmation, LowerCase),
        ( Confirmation \= "", LowerCase \= "n" -> 
            movie_operations:avaluateMovie(Connection, User, Movie, Rating, Comment, Conf),
            ( Conf =:= 1 -> clear_screen,
                writeln("Avaliação feita com sucesso!");
                clear_screen,
                writeln("Um mesmo usuário não pode avaliar o mesmo filme duas vezes!")
            )
            ;
            writeln("Ok!" ),
            clear_screen_with_confirmation    
        )
        ;
        writeln("Digite um valor válido na próxima vez!"), newRating(Connection, User, Movie)
    ).


printRatings(Connection, Movie):-
    getRatingsMovies(Connection, Movie, Ratings),
    ( Ratings = [] -> writeln(""), writeln("Esse filme ainda não foi avaliado por algum usuário!"), clear_screen_with_confirmation;
        writeln(""),
        writeln("------------------------------------------------"),
        writeln("                  Avaliações"),
        writeln("------------------------------------------------"),
        writeln(""),
        printRatingsAux(Ratings), clear_screen_with_confirmation
    ).


printRatingsAux([]).
printRatingsAux([row(_, Email, _, Rating, Comment) | T]):-
    write(Email), write(" - "), write(Rating), write(" - "), writeln(Comment),
    printRatingsAux(T).