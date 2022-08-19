:- module(dbop, [get_connection/1, db_query/3, db_parameterized_query/4, db_parameterized_query_no_return/3]).
:- use_module(library(odbc)).

get_connection(Connection) :-
    odbc_connect('SWI-Prolog', Connection, []).


db_query(Connection, Query, Rows) :-
    findall(
        Result,
        odbc_query(Connection, Query, Result),
        Rows
    ).

db_query_no_return(Connection, Query) :-
    odbc_query(Connection, Query).
    

db_parameterized_query(Connection, Query, Parameters, Rows):-
    swritef(String, Query, Parameters),
    db_query(Connection, String, Rows).
    

db_parameterized_query_no_return(Connection, Query, Parameters):-
    swritef(String, Query, Parameters),
    db_query_no_return(Connection, String).