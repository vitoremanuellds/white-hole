:- module(dbop, [get_connection/1, db_query/3]).
:- use_module(library(odbc)).

get_connection(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).


db_query(Connection, Query, Rows) :-
    findall(
        Result,
        odbc_query(Connection, Query, Result),
        Rows
    ).
    

db_parameterized_query(Connection, Query, Parameters, Rows):-
    swritef(String, Query, Parameters),
    db_query(Connection, String, Rows).
    