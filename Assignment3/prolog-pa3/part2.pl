:- [hogwarts].
:- [part2Helper].

% Hints:
%   for NLP parser make sure you match the type of the atom: 
%      it may come in handy that print and write are two different method of printing within prolog 

% Main 
main :-
    current_prolog_flag(argv, [DataFile|_]),
    open(DataFile, read, Stream),
    read_file(Stream, Lines),
    process_lines(Lines),
    close(Stream).

% Process each line in the list
process_lines([]).
process_lines([Line|Rest]) :-
    process_line(Line),
    process_lines(Rest).

% Define how to process each line
process_line(Line) :-
    ( is_item_line(Line) -> handle_item(Line)
    ; is_house_line(Line) -> handle_house(Line)
    ; true
    ).


% Check if a line corresponds to an item
is_item_line([_Person, has, item, _Item, that, costs, _Cost, dollars, ',', and, occupies, _Volume, cubic, feet]).


% Check if a line corresponds to a house constraint
is_house_line([_House, house, wants, _, _, _, _, and, _, _, _, _]).

handle_item([_Person, has, item, Item, that, costs, Cost, dollars, and, occupies, Volume, cubic, feet]) :-
    write(here).

% Define how to handle house lines
handle_house([House, house, wants, Attribute1, Comparison1, Value1, Unit1, and, Attribute2, Comparison2, Value2, Unit2]) :-
   write(here).