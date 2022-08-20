:- module(load, [load/0]).
:- use_module("./Db/dbOperations.pl").
:- use_module("./Operations/interface_operations.pl").
:- use_module("./Operations/movie_operations.pl").
:- use_module("./Operations/serie_operations.pl").
:- use_module("./Operations/util.pl").

load :-
    clear_screen,
    writeln(""),
    writeln("Conectando com a base de dados... (Aguarde)"),
    get_connection(Connection),
    clear_screen,
    writeln("Recuperando informações sobre filmes... (Aguarde)"),
    clear_screen,
    getMoviesWithRatings(Connection, Movies),
    writeln("Recuperando informações sobre séries... (Aguarde)"),
    clear_screen,
    getSeriesWithRatings(Connection, Series),
    writeln("Atualizando informações sobre filmes... (Aguarde)"),
    clear_screen,
    updateAllMovieRating(Connection, Movies),
    writeln("Atualizando informações sobre séries... (Aguarde)"),
    clear_screen,
    updateAllSerieRating(Connection, Series),
    writeln("Tudo pronto!"),
    clear_screen_with_confirmation,
    first_menu(Connection).
