:- module(user_operations, [createUser/6, getUserByEmail/3, userAlreadyExists/3, authenticate/4, getUsersByEmail/4]).
:- use_module('./util.pl').
:- use_module('../Db/dbOperations.pl').


createUser(Connection, Email, Senha, Nome, Sobrenome, Confirmacao):-
    userAlreadyExists(Connection, Email, Fstconf),
    ( Fstconf =:= 0 ->
        db_parameterized_query_no_return(
            Connection, 
            "INSERT INTO users (email, passwd, nome, sobrenome) values ('%w','%w','%w','%w');",
            [Email, Senha, Nome, Sobrenome]),
        Confirmacao is 1;
        Confirmacao is 0).


getUserByEmail(Connection, Email, User):-
    Q = "SELECT * FROM users WHERE email = '%w'",
    db_parameterized_query(Connection, Q, [Email], User).


userAlreadyExists(Connection, Email, Confirmacao):-
    getUserByEmail(Connection, Email, User),
    length(User, Confirmacao).


authenticate(Connection, Email, Senha, Confirmacao):-
    getUserByEmail(Connection, Email, User),
    length(User, L),
    (L =:= 0 ->
        Confirmacao is 0;
        User = [ row(_, SenhaReal, _, _) | _ ],
        (Senha = SenhaReal -> Confirmacao is 1; Confirmacao is 0)
    ).


getUsersByEmail(Connection, [ Email | T ], UsersTemp, Users):-
    length(T, L),
    (L > 0 ->
        getUserByEmail(Connection, Email, User),
        getUsersByEmail(Connection, T, [ User | UsersTemp ], Users) ;
        getUserByEmail(Connection, Email, User),
        reverse([ User | UsersTemp ], Users)
    ).