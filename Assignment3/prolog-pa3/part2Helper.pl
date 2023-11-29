

% Split the sentence into list
split_with_two_separators(String, Result) :-
    atomic_list_concat(Parts1, " ", String), 
    split_parts(Parts1,".", Parts2),    
    split_parts(Parts2,",", Result).     

split_parts([],_,[]).
split_parts([Part|Rest],Seperator, Result) :-
    atomic_list_concat(Parts2, Seperator, Part),   
    append(Parts2, RestOfResult, Result),            
    split_parts(Rest, Seperator, RestOfResult).


% Read from file
read_file(Stream,[]) :- 
    at_end_of_stream(Stream),!.

read_file(Stream,[String_List|L]) :-
   %\+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    string_codes(String_Line,Line),
	split_with_two_separators(String_Line,String_List),
    read_file(Stream,L). 