
:- [hogwarts].
:- [relations].


% Define your relations and characters here.
% Define the PrintTestResult predicate to display the results.
printTestResult(Condition) :-
    (Condition ->
        write('Passed\n')
    ;
        write('Failed\n')
    ).
len([], LenResult):-
    LenResult is 0.
len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.    
match(L1,L2) :- (member(E,L1),member(E,L2)),len(L1, T),  len(L2, T) .
negative(Condition) :-
    (Condition ->
        false
    ;
        true 
    ).

test1 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 1: Harry Potter and Ron Weasley are classmates: '),
    classmates(ron_weasley, harry_potter),
    classmates(harry_potter,ron_weasley).

test2 :-
    write('Test 2: Harry Potter and Draco Malfoy live far away: '),
    liveFarAway(draco_malfoy, harry_potter),
    liveFarAway(harry_potter,draco_malfoy).

test3 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 3: Albus Dumbledore is senior to Ginny Weasley: '),
    isSeniorOf(albus_dumbledore, ginny_weasley).

test4 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 4: Seniors of James Potter are Minerva Mcgonagall and Albus Dumbledore: '),
    findall(A,listSeniors(james_potter, A), [Bag]),
    match(Bag,[albus_dumbledore,minerva_mcgonagall]).

test5 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 5: Juniors of Bill Weasley are Fred Weasley, Ron Weasley and Ginny Weasley: '),
    findall(A,listJuniors(bill_weasley, A), [Bag]),
    match(Bag,[fred_weasley,ron_weasley ,ginny_weasley]).

test6 :-
    % Define the rest of the tests in a similar fashion.
    write( 'Test 6: Oldest student in the House Gryffindor is Alicia Spinnet: '),
    oldestStudent(alicia_spinnet, gryffindor).

test7 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 7: Youngest student in the House Gryffindor is Ginny Weasley: '),
    youngestStudent(ginny_weasley, gryffindor).

test8 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 8: Oldest student in the Hufflepuff team is David Smith: '),
    oldestQuidditchStudent(hufflepuff_team, david_smith).

test9 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 9: Youngest student in the Hufflepuff team is Eli Olivander: '),
    youngestQuidditchStudent(hufflepuff_team, eli_olivander).

test10 :-
    % Define the rest of the tests in a similar fashion.
    write( 'Test 10: Harry Potter and Draco Malfoy are rivals: '),
    rival(harry_potter,draco_malfoy),
    rival(draco_malfoy,harry_potter).
test11 :-
    % Define the rest of the tests in a similar fashion.
    write('Test 11: Harry Potter and Draco Malfoy are not only rivals but also live far away from each other: '),
    farRival(harry_potter,draco_malfoy).



main:- 
    printTestResult(test1),
    printTestResult(test2),
    printTestResult(test3),
    printTestResult(test4),
    printTestResult(test5),
    printTestResult(test6),
    printTestResult(test7),
    printTestResult(test8),
    printTestResult(test9),
    printTestResult(test10),
    printTestResult(test11).