:- module(util, [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).

get_input(Prompt, Input) :- 
    writeln(Prompt),
    read_string(user_input, "\n", "\t ", _, Input).

clear_screen :- write('\e[2J\e[H\e[3J').

clear_screen_with_confirmation :- writeln(''), get_input('(Aperte Enter)', Input), clear_screen.

isANumber(Number,String):- number_string(Number, String).

splitItems(String, Items):- split_string(String, ",", " ", Items).

capitalize(String, Result):- string_upper(String, Result).

concatenate_with_comma(A,C):- atomic_list_concat(A,', ', C).


convertCategories([CatNum|T], ResultTemp, Result):-
    length(T, L),
    (L > 0 ->
        Categories = ["ação","suspense","romance","comédia","terror","aventura","investigação","fantasia","documentário","drama","anime","mistério","infantil","ficção científica"],
        nth1(CatNum, Categories, Category),
        convertCategories(T, [Category | ResultTemp], Result)
        ;
        Categories = ["ação","suspense","romance","comédia","terror","aventura","investigação","fantasia","documentário","drama","anime","mistério","infantil","ficção científica"],
        nth1(CatNum, Categories, Category),
        reverse(ResultTemp, Result)
    ).


verify_cartegory([]).
verify_cartegory([X|XS]):- verify_cartegory(XS), number_string(X, Y), Y < 15, Y > 0.

strip(A,B):- split_string(A, "", "\s\t\n", [B]).