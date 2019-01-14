% room 
% ID,capacity,equipment

room(z06,10,[hcapped,projector]).
room(z11,10,[hcapped,smartboard]).

% occupancy
% ID , hour,course

occupancy(z06,8,cse341).
occupancy(z06,9,cse341).
occupancy(z06,10,cse341).
occupancy(z06,11,cse341).
occupancy(z06,12,empty). % empty room
occupancy(z06,13,cse331).
occupancy(z06,14,cse331).
occupancy(z06,15,cse331).
occupancy(z06,16,empty). % empty room
occupancy(z11,8,cse343).
occupancy(z11,9,cse343).
occupancy(z11,10,cse343).
occupancy(z11,11,cse343).
occupancy(z11,12,empty).
occupancy(z11,13,empty).
occupancy(z11,14,cse321).
occupancy(z11,15,cse321).
occupancy(z11,16,cse321).

% course
% ID, teacher , capacity, hour,room

course(cse341,genc,10,4,z06).
course(cse343,turker,6,3,z11).
course(cse331,bayrakci,5,3,z06).
course(cse321,gozupek,10,4,z11).


% student 
% studentID,courses,hcapped
student(1,[cse341,cse343,cse331],no).
student(2,[cse341,cse343],no).
student(3,[cse341,cse331],no).
student(4,[cse341],no).
student(5,[cse341,cse331],no).
student(6,[cse341,cse343,cse331],yes).
student(7,[cse341,cse343,cse331],no).
student(8,[cse341,cse331],yes).
student(9,[cse341,cse343,cse331],no).
student(10,[cse341,cse321],no).
student(11,[cse341,cse321],no).
student(12,[cse341,cse321],no).
student(13,[cse341,cse321],no).
student(14,[cse341,cse321],no).
student(15,[cse341,cse321],yes).



% instructor
% ID,course,need
teacher(genc,cse341,projector).
teacher(turker,cse343,smartboard).
teacher(bayrakci,cse331,empty).
teacher(gozupek,cse321,smartboard).


% checks if in list  
member(X, [Y|T]) :- X = Y ; member(X, T).


% add elements to list

addElement(X, [], [X]). 
addElement(X, [Y | Rest], [X,Y | Rest]) :- X @< Y, !.
addElement(X, [Y | Rest1], [Y | Rest2]) :- addElement(X, Rest1, Rest2).


%% SORGU 1 
%Check whether there is any scheduling conflict. --> conflicts(CourseID1,CourseID2)
% calisan ornek = conflict(cse331,cse321).

class(X,Y):- occupancy(A,_,X),
                occupancy(B,_,Y),
                A==B.

time(X,Y):- occupancy(_,A,X),
                occupancy(_,B,Y),
                A==B.
conflicts(X,Y):- not((not(class(X,Y)),
                     not(time(X,Y)))).

%% SORGU 2
%Check which room can be assigned to a given class. --> assign(RoomID,CourseID)
% dersin handicapli veya board istiyo mu ona bayrak
% capacity yeter mi ona bak

% calısan ornek = assign(z11,cse343)
% which need smart board which z11 has

% also capacity is enough to hold class.
 % A>=B
assign1(RoomID,CourseID) :-
teacher(_,CourseID,X),
room(RoomID,A,Y),
member(X,Y),
course(CourseID,_,B,_,_),
A>=B.

% X elemanı Y listesinde var mı diye bakması lazım


% SORGU 3 

% Check which room can be assigned to which classes. --> assign(RoomID,_)
% ornek kullanım = assign2(z06,X) sonra ; ; basarak degerleri gorebilirsnz
assign2(RoomID,X) :- 
assign2_helper(RoomID,X);
assign2_helper2(RoomID,X).

assign2_helper(RoomID,X) :-
room(RoomID,_,Y),
course(X,_,_,_,RoomID),
teacher(_,X,B),
member(B,Y). %% if teacher needs - board, projector fits the room we are gonna predict

assign2_helper2(RoomID,X) :-
room(RoomID,_,Y),
course(X,_,_,_,RoomID),
teacher(_,X,B),
member(B,[empty]). %% if teacher needs - board, projector fits the room we are gonna predict
% empty ise direk gecsin


% SORGU 4
% Check whether a student can be enrolled to a given class. --> enroll(StudentID,CourseID)
% ornek kullanım enroll(1,cse343).
enroll(StudentID,CourseID) :-
student(StudentID,X,_),
member(CourseID,X).

%SORGU 5
%Check which classes a student can be assigned. --> enroll2(StudentID,X)
% ornek enroll2(1,X).
enroll2(StudentID,X):-
student(StudentID,X,_).

%odev soruları
% occCheck([X],CourseID1,CourseID2) :- 
%conflicts(CourseID1,CourseID2) :- 
%((occupancy(z06,8,CourseID1),occupancy(z11,8,CourseID1));
%(occupancy(z06,9,CourseID1),occupancy(z11,9,CourseID1));
%(occupancy(z06,10,CourseID1),occupancy(z11,10,CourseID1));
%(occupancy(z06,11,CourseID1),occupancy(z11,11,CourseID1));
%(occupancy(z06,12,CourseID1),occupancy(z11,12,CourseID1));
%(occupancy(z06,13,CourseID1),occupancy(z11,13,CourseID1));
%(occupancy(z06,14,CourseID1),occupancy(z11,14,CourseID1));
%(occupancy(z06,15,CourseID1),occupancy(z11,15,CourseID1));
%(occupancy(z06,16,CourseID1),occupancy(z11,16,CourseID1))).

