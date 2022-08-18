:- module(interfaceop, [first_menu/1]).
:- use_module('./util.pl', [get_input/2, clear_screen/0, clear_screen_with_confirmation/0]).

first_menu(Connection) :-
    writeln(''),
    writeln('Bem vindo ao White Hole de filmes e séries!'),
    writeln('Descubra novos filmes para assistir e avalie os que você já assistiu!'),
    writeln(''),
    writeln('1 - Logar no White Hole'),
    writeln('2 - Cadastrar-se no White Hole'),
    writeln('3 - Sair'),
    writeln(''),
    get_input('Digite a opção que deseja acessar:', Option),
    first_menu_options(Connection, Option).

first_menu_options(Connection, "1") :- clear_screen, writeln('signin'), first_menu(Connection).
first_menu_options(Connection, "2") :- clear_screen, writeln('signup'), first_menu(Connection).
first_menu_options(Connection, "3") :- clear_screen, writeln('Obrigado, tenha um ótimo dia!'), clear_screen_with_confirmation, halt.
first_menu_options(Connection, X) :- clear_screen, writeln('Digite uma opção válida na próxima!'), writeln(''), clear_screen_with_confirmation, first_menu(Connection).