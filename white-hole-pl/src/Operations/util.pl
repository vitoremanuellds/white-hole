:- module(util, [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).

get_input(Prompt, Input) :- 
    writeln(Prompt),
    read_string(user_input, "\n", "\t ", _, Input).

clear_screen :- write('\e[2J\e[H\e[3J').

clear_screen_with_confirmation :- writeln(''), get_input('(Aperte Enter)', Input), clear_screen.