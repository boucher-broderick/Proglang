
:- [hogwarts].

studentOf(Student, Teacher) :-
    teacherOf(Teacher, Student).

classmates(StudentOne, StudentTwo):-     
    teacherOf(Teacher, StudentOne),
    teacherOf(Teacher, StudentTwo),
    StudentOne \= StudentTwo.

liveFarAway(StudentOne, StudentTwo):-     
    houseOf(HouseOne, StudentOne),
    houseOf(HouseTwo, StudentTwo),
    HouseOne \= HouseTwo,
    farLocation(HouseOne, HouseTwo).

isSeniorOf(PersonA, PersonB) :-
    directSeniorOf(PersonA, PersonB).

isSeniorOf(PersonA, PersonB) :-
    directSeniorOf(PersonA, PersonC),
    isSeniorOf(PersonC, PersonB).

listSeniors(Person, Seniors):- 
    findall(Senior, isSeniorOf(Senior, Person), Seniors).

listJuniors(Person, Juniors):- 
    findall(Junior, isSeniorOf(Person, Junior), Juniors).


oldestStudent(Person, House):- 
    findall(BirthYear-Student, (houseOf(House, Student), birthYear(Student, BirthYear)), StudentsWithYears),
    sort(StudentsWithYears, SortedStudentsWithYears),
    SortedStudentsWithYears = [Oldest|_],
    Oldest = _-Person.

youngestStudent(Person, House):- 
    findall(BirthYear-Student, (houseOf(House, Student), birthYear(Student, BirthYear)), StudentsWithYears),
    sort(StudentsWithYears, SortedStudentsWithYears),
    last(SortedStudentsWithYears, _-Person).


oldestQuidditchStudent(Team, Student):- 
    findall(BirthYear-TeamMember, (quidditchTeamOf(Team, TeamMember), birthYear(TeamMember, BirthYear)), TeamMembersWithYears),
    sort(TeamMembersWithYears, SortedTeamMembersWithYears),
    SortedTeamMembersWithYears = [Oldest|_],
    Oldest = _-Student.

youngestQuidditchStudent(Team, Student):-
    findall(BirthYear-TeamMember, (quidditchTeamOf(Team, TeamMember), birthYear(TeamMember, BirthYear)), TeamMembersWithYears),
    sort(TeamMembersWithYears, SortedTeamMembersWithYears),
    last(SortedTeamMembersWithYears, _-Student).

rival(StudentOne, StudentTwo):- 
    houseOf(HouseOne, StudentOne),
    houseOf(HouseTwo, StudentTwo),
    HouseOne \= HouseTwo.

farRival(StudentOne, StudentTwo):- 
    houseOf(HouseOne, StudentOne),
    houseOf(HouseTwo, StudentTwo),
    HouseOne \= HouseTwo,
    farLocation(HouseOne, HouseTwo).
