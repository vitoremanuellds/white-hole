:- module(serie_interface_operations, [
        createSerie/1,
        tenBestSeries/2,
        printSeriesList/2
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


printSeriesList([Serie | T], _).
printSeriesList([Serie | T], Num):-
    Serie = row(_, Title, _, _, _, _),
    util:capitalize(Title, CapitalizedTitle),
    swritef("%w  - %w", [Num, CapitalizedTitle], Saida),
    writeln(Saida),
    printSeriesList(T, Num + 1).


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
        convertCategories([NOption], [], Categories),
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


showSerie :: Connection -> User -> S.Serie -> IO ()
showSerie conn user serie = do
    writeln(""),
    writeln("-------------------------------------------------"),
    writeln(("     " ++ capitalize (S.title serie))),
    writeln("-------------------------------------------------"),
    writeln(""),
    writeln(("Sinopse: " ++ S.summary serie)),
    writeln(""),
    writeln(("Quantidade de episódios: " ++ S.episodes serie)),
    writeln(("Data de lançamento: " ++ S.releaseDate serie)),
    categories <- getCategoriesOfSeriesInOneString conn serie
    writeln(("Categorias: " ++ capitalize categories)),
    writeln(""),
    writeln(("Nota geral: " ++ show (S.rating serie))),
    writeln(""),
    writeln("1 - Mostrar casting da série."),
    writeln("2 - Marcar como assistir depois."),
    writeln("3 - Avaliar série."),
    writeln("4 - Mostrar avaliações da série."),
    writeln("5 - Voltar."),
    writeln(""),
    writeln("Digite a opção desejada: "),
    hFlush stdout
    option <- getLine
    case option of
        "1" -> clearScreenOnly >> printCasting conn serie >> showSerie conn user serie
        "2" -> clearScreenOnly >> addToWatchLaterListSeries conn user serie >> writeln("Filme adicionado com sucesso na lista!" >> clearScreenWithConfirmation >> showSerie conn user serie),
        "3" -> clearScreenOnly >> newRating conn user serie >> updateSerieRating conn serie >> showSerie conn user serie
        "4" -> clearScreenOnly >> printRatings conn serie >> showSerie conn user serie
        "5" -> clearScreenOnly 
        x -> writeln("Digite uma opção válida" >> clearScreenWithConfirmation >> showSerie conn user serie),



printCasting :: Connection -> S.Serie -> IO ()
printCasting conn serie = do
    casting <- getCastingSerie conn serie
    if null casting then do 
        writeln("" >> writeln"Não há um Casting cadastrado para essa série!" >> clearScreenWithConfirmation),
    else do
        clearScreenOnly
        writeln("------------------------------------------------"),
        writeln("                    Casting"),
        writeln("------------------------------------------------"),
        writeln(""),
        printCasting' casting >> clearScreenWithConfirmation


printCasting' :: [(String, String)] -> IO ()
printCasting' [] = return ()
printCasting' (x:xs) = do
    writeln(("* " ++ fst x ++ " - " ++ capitalize (snd x))),
    printCasting' xs


newRating :: Connection -> User -> S.Serie -> IO ()
newRating conn user serie = do
    writeln(""),
    writeln("Dê uma nota para a série (De 1 a 5): "),
    nota <- getLine
    if not (isANumber nota True && (read nota :: Int) `elem` [1..5]) then writeln("Digite um valor válido na próxima vez!" >> newRating conn user serie else writeln""),
    writeln("Faça um comentário sobre a série (Se não quiser, basta apertar enter): "),
    commentary <- getLine
    let comment = if null commentary then "Sem comentário" else commentary
    writeln(""),
    writeln("Confirmar avaliação (s/N)"),
    confirmation <- getLine
    if null confirmation || map toLower confirmation == "n" then do 
        writeln("Ok!" ),
        clearScreenWithConfirmation
    else do 
        confirmation <- avaluateSerie conn user serie (read nota :: Integer) comment
        if confirmation then do
            clearScreenOnly
            writeln("Avaliação feita com sucesso!"),
        else do
            clearScreenOnly
            writeln("Um mesmo usuário não pode avaliar a mesma série duas vezes!"),
        clearScreenWithConfirmation


printRatings :: Connection -> S.Serie -> IO ()
printRatings conn serie = do
    ratings <- getRatingsSeries conn serie
    if null ratings then do 
        writeln("" >> writeln"Essa série ainda não foi avaliada por algum usuário!" >> clearScreenWithConfirmation),
    else do 
        writeln(""),
        writeln("------------------------------------------------"),
        writeln("                  Avaliações"),
        writeln("------------------------------------------------"),
        writeln(""),
        printRatings' ratings >> clearScreenWithConfirmation


printRatings' :: [R.Rating] -> IO ()
printRatings' [] = return ()
printRatings' (x:xs) = do
    writeln((R.userEmail x ++ " - " ++ show (R.rating x) ++ " - " ++ R.commentary x)),
    printRatings' xs.
