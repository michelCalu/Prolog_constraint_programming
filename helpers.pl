length_(Length, List) :- length(List, Length).


create(NbLists, LengthList, Res):-
    length(Res, NbLists),
    maplist(length_(LengthList), Res).
