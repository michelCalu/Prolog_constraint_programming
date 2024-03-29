%-----------------------------------------------------------
% creates a list of 'NbLists' lists of 'LengthList' elements
%-----------------------------------------------------------
create(NbLists, LengthList, Res):-
    length(Res, NbLists),
    maplist(length_(LengthList), Res).

length_(Length, List) :- length(List, Length).


%------------------------------------------------------------
% creates a list of 'Size' variables and labels them
%------------------------------------------------------------
go(List,Size):-
    length(List, Size),
    List ins 1..4,
    all_different(List),
    List=[1|T],
    label(List).

%------------------------------------------------------------
% tuples_in(+ListOfVariableTuples, +ListOfAcceptedTuples)
%------------------------------------------------------------
    go(X,Y):-
        tuples_in([[X,Y]], [[1,3],[2,4],[9,6],[5,9]]),
        X#>Y.

%------------------------------------------------------------
% maplist(+Pred, ?List1, ?List2).
% Appelle le prédicat Pred (d'arité 2 au min) avec comme arguments les membres de List1 et List2.
%------------------------------------------------------------
maplist( square, [1,2,3] ,[1,4,9]).
square(N,R) :- R is N*N.


%------------------------------------------------------------
%
%------------------------------------------------------------
%We will find many other uses for 'member'. This example query ...

?- member([3,Y], [[1,a],[2,m],[3,z],[4,v],[3,p]]).
Y = z ;
Y = p ;
No


%------------------------------------------------------------
% Count number of Solutions of go/1 in Count
%------------------------------------------------------------

setof([Solutions],go(Solutions), List), length(List, Count).


%------------------------------------------------------------
%
%------------------------------------------------------------
