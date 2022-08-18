:- module(load, [load/0]).
:- use_module("./Db/dbOperations.pl").
:- use_module("./Operations/interface_operations.pl").

load :-
    get_connection(Connection),
    first_menu(Connection).