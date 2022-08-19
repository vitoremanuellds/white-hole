:- module(serie_interface_operations, [
        createSerie/1
    ]).
:- use_module('./util.pl').
:- use_module('../Db/dbOperations.pl').
:- use_module('./user_operations.pl').


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
                Summary,
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
                        addCastingToSerie(Connection, Serie, ActorsAdd),
                        util:splitItems(Directors, DirectorsAdd),
                        addDirectorsToSerie(Connection, Serie, DirectorsAdd),
                        util:convertCategories(SplitedCategories, [], CategoriesConverted),
                        addCategoriesToSerie(Connection, Serie, CategoriesConverted),
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