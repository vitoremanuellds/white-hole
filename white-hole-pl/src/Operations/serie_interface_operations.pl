:- module(serie_interface_operations, [
        createSerie/1,
        tenBestSeries/2,
        printSeriesList/2,
        tenBestSeriesByCategory/2,
        showSerie/3,
        printCasting/2,
        printCastingAux/1,
        newRating/3,
        printRatings/2,
        printRatingsAux/1
    ]).
:- use_module('./util.pl').
:- use_module('../Db/dbOperations.pl').
:- use_module('./user_operations.pl').
:- use_module('./serie_operations.pl').


createSerie(Connection):-
    writeln(""),
    get_input("Qual o título da série?", X),
    util:strip(X, Title),
    (Title = "" -> writeln("A série deve ter um título!"), 
        clear_screen_with_confirmation;
        get_input("Qual a data de lançamento da série? (yyyy-mm-dd)", Y),
        util:strip(Y, ReleaseDate),
        (ReleaseDate = "" -> writeln("A data deve ser uma data válida!"),
            clear_screen_with_confirmation;
            get_input("Qual a quantidade de episódios da série?", Z),
            util:strip(Z, Episodes),
            ( Episodes \= "", util:isANumber(NEpisodes, Episodes), NEpisodes < 10000 ->
                get_input("Qual a sinopse da série?", Sinopse),
                get_input("Digite o nome dos diretores (separados por vírgula):", Directors),
                get_input("Digite o nome dos atores (separados por vírgula):", Actors),
                ( Sinopse = "" -> Summary = "Sem sinopse"; Summary = Sinopse),
                writeln(""),
                writeln("Digite a quais categorias essa série pertence (digite os números associados separados por espaços):"),
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
                    get_input("Quer realmente adicionar essa série? (s/N)", Confirmation),
                    string_lower(Confirmation, LowerCase),
                    ( Confirmation \= "", LowerCase \= "n" -> 
                        writeln(""),
                        writeln("Ok!"),
                        clear_screen_with_confirmation;
                        registerSerie(Connection, Title, ReleaseDate, Episodes, Summary, Serie),
                        util:splitItems(Actors, ActorsAdd),
                        serie_operations:addCastingToSerie(Connection, Serie, ActorsAdd, _),
                        util:splitItems(Directors, DirectorsAdd),
                        serie_operations:addDirectorsToSerie(Connection, Serie, DirectorsAdd, _),
                        util:convertCategories(SplitedCategories, [], CategoriesConverted),
                        serie_operations:addCategoriesToSeries(Connection, Serie, CategoriesConverted, _),
                        writeln(""),
                        writeln("Série cadastrada com sucesso!"),
                        clear_screen_with_confirmation
                    )    
                    ;
                    writeln("É necessário que a série tenha ao menos uma categoria!"),
                    clear_screen_with_confirmation
                )
                ;
                writeln("Digite uma quantidade de episódios válida"),
                clear_screen_with_confirmation
            )
        )
    ).


printSeriesList([], _).
printSeriesList([Serie | T], Num):-
    Serie = row(_, Title, _, _, _, _),
    util:capitalize(Title, CapitalizedTitle),
    swritef(Saida, "%w  - %w", [Num, CapitalizedTitle]),
    writeln(Saida),
    NextNum is Num + 1,
    printSeriesList(T, NextNum).


tenBestSeries(Connection, User):-
    writeln(""),
    writeln("As dez séries mais bem avaliadas no momento são: "),
    writeln(""),
    getTenBestSeries(Connection, Series),
    printSeriesList(Series, 1),
    writeln(""),
    get_input("Digite o número da séries para acessá-la (aperte enter para voltar):", Option),
    length(Series, SLength),
    (Option = "" -> clear_screen;
        ( util:isANumber(NOption, Option), NOption < SLength + 1, NOption > 0 ->
            clear_screen,
            nth1(NOption, Series, Serie),
            showSerie(Connection, User, Serie),
            tenBestSeries(Connection, User);
            writeln(""),
            writeln("Digite uma opção válida na próxima vez."),
            clear_screen_with_confirmation
        )  
    ).


tenBestSeriesByCategory(Connection, User):-
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
        writeln("As dez séries mais bem avaliadas dessa categoria no momento são (Se a categoria não contiver filmes o suficiente, pode não aparecer dez séries): "),
        util:convertCategories([NOption], [], Categories),
        getSeriesByCategory(Connection, Categories, Series),
        writeln(""),
        printSeriesList(Series, 1),
        writeln(""),
        get_input("Digite o número da série para acessá-la (aperte enter para voltar):", Input),
        util:strip(Input, InputTrimmed),

        ( InputTrimmed = "" -> clear_screen;
            InputTrimmed \= "", util:isANumber(NInput, InputTrimmed), NInput < 15, NInput > 0 ->
            clear_screen,
            nth1(NInput, Series, Serie),
            showSerie(Connection, User, Serie),
            tenBestSeriesByCategory(Connection, User)
        );
        writeln(""),
        writeln("Digite uma opção válida na próxima vez."),
        clear_screen_with_confirmation
    ).


showSerie(Connection, User, Serie):-
    Serie = row(_, Title, RDate, Episodes, Summary, Rating),
    util:capitalize(Title, CTitle),
    writeln("-------------------------------------------------"),
    writeln(CTitle),
    writeln("-------------------------------------------------"),
    writeln(""),
    swritef(Sinopse, "Sinopse: %w", [Summary]),
    writeln(Sinopse),
    writeln(""),
    write("Quantidade de episódios: "), writeln(Episodes),
    writeln("Data de lançamento: "), writeln(RDate),
    getCategoriesOfSeriesInOneString(Connection, Serie, CategoriesString),
    write("Categorias: "), writeln(CategoriesString),
    writeln(""),
    write("Nota geral: "), writeln(Rating),
    writeln(""),
    writeln("1 - Mostrar casting da série."),
    writeln("2 - Marcar como assistir depois."),
    writeln("3 - Avaliar série."),
    writeln("4 - Mostrar avaliações da série."),
    writeln("5 - Voltar."),
    writeln(""),
    get_input("Digite a opção desejada: ", Option),
    ( Option = "1" -> clear_screen, printCasting(Connection, Serie), showSerie(Connection, User, Serie);
    Option = "2" -> clear_screen, serie_operations:addToWatchLaterListSeries(Connection, User, Serie, _), writeln("Filme adicionado com sucesso na lista!"), clear_screen_with_confirmation, showSerie(Connection, User, Serie);
    Option = "3" -> clear_screen, newRating(Connection, User, Serie), serie_operations:updateSerieRating(Connection, Serie, _), showSerie(Connection, User, Serie);
    Option = "4" -> clear_screen, printRatings(Connection, Serie), showSerie(Connection, User, Serie);
    Option = "5" -> clear_screen;
        writeln("Digite uma opção válida"), clear_screen_with_confirmation, showSerie(Connection, User, Serie)
    ).
        

printCasting(Connection, Serie):-
    getCastingSerie(Connection, Serie, Casting),
    ( Casting = [] -> writeln(""), writeln("Não há um Casting cadastrado para essa série!"), clear_screen_with_confirmation;
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


newRating(Connection, User, Serie):-
    writeln(""),
    get_input("Dê uma nota para a série (De 1 a 5): ", Rating),
    ( util:isANumber(N, Rating), member(N, [1,2,3,4,5]) ->
        writeln(""),
        get_input("Faça um comentário sobre a série (Se não quiser, basta apertar enter): ", C),
        ( C = "" -> Comment = "Sem comentario" ;
            Comment = C
        ),
        writeln(""),
        get_input("Confirmar avaliação (s/N)", Confirmation),
        string_lower(Confirmation, LowerCase),
        ( Confirmation \= "", LowerCase \= "n" -> 
            serie_operations:avaluateSerie(Connection, User, Serie, Rating, Comment, Conf),
            ( Conf =:= 1 -> clear_screen,
                writeln("Avaliação feita com sucesso!");
                clear_screen,
                writeln("Um mesmo usuário não pode avaliar a mesma série duas vezes!")
            )
            ;
            writeln("Ok!" ),
            clear_screen_with_confirmation    
        )
        ;
        writeln("Digite um valor válido na próxima vez!"), newRating(Connection, User, Serie)
    ).


printRatings(Connection, Serie):-
    getRatingsSeries(Connection, Serie, Ratings),
    ( Ratings = [] -> writeln(""), writeln("Essa série ainda não foi avaliada por algum usuário!"), clear_screen_with_confirmation;
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

