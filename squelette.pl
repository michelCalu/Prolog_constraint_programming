% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space
:- style_check(- singleton).
:- use_module(library(clpfd)). % librairie de programmation logique
                               % par contrainte à domaine fini

testNoms([  anne, michel, manon, alice, juliette, lucie, charlotte, olivia, margaux,
            jules, hugo, tom, louis, paul, jean, antoine  ]).

go(Lassoc) :-
    testNoms(L),
    length(L,Nb),
    produire_liste(L,Lassoc),
    term_variables(Lassoc,Labels),
    all_different(Labels),
    mme_mr(Lassoc,Nb),
    alternate(Lassoc),
    label(Labels),
    write(Labels).

mme_mr([Mme|T],Nb):-
    getvar(Mme,VarMme),
    1 #=  VarMme,
    nth0(0,T,Mr),
    getvar(Mr,VarMr),
    Mid is Nb/2,
    Mid #= VarMr.

alternate([]).
alternate([H]).
alternate([H|T]):-
    get_suivant(T,Suivant),!,
    getname(H,N1),
    getname(Suivant,NameSuiv),
    genre_compatible(N1,NameSuiv),
    alternate(T).

get_suivant([],[]) .
get_suivant([H|T],H) .


getname(PersVar,Name):-
    Persvar=(Name,_).

genre_compatible(Name1, Name2) :-
    sexe(Name1, Sexe1),!, sexe(Name2,Sexe2),not(Sexe1=Sexe2).


getvar(PersVar,Var):-
    PersVar=pers_var(_,Var).

produire_liste([],[]).
produire_liste([H|T],[pers_var(H,Var)|Rest]):-
    Var in 1..16,
    produire_liste(T,Rest).

getlist([], []).
getlist([H|T], [Var|Rest]):-
    X=pers_var(N,Var),
    getlist(T,Rest).

makenew([], []).
makenew([H|T], [pers_var(N,H)|Rest]):-
        makenew(T,Rest).


epoux(anne,michel).
epoux(manon,jules).
epoux(alice,hugo).
epoux(juliette,tom).
epoux(lucie,louis).
epoux(charlotte,paul).
epoux(olivia,jean).
epoux(margaux,antoine).

sexe(anne,f).
sexe(manon,f).
sexe(alice,f).
sexe(juliette,f).
sexe(lucie,f).
sexe(charlotte,f).
sexe(olivia,f).
sexe(margaux,f).

sexe(michel,m).
sexe(jules,m).
sexe(hugo,m).
sexe(tom,m).
sexe(louis,m).
sexe(paul,m).
sexe(jean,m).
sexe(antoine,m).

hobby(anne,[sport,jardinage,voyages]).
hobby(manon,[jardinage,voyages]).
hobby(alice,[jardinage,voyages]).
hobby(juliette,[sport,voyages]).
hobby(lucie,[jardinage]).
hobby(charlotte,[jardinage]).
hobby(olivia,[sport,jardinage]).
hobby(margaux,[sport]).

hobby(michel,[sport,jardinage,voyages]).
hobby(jules,[sport,jardinage]).
hobby(hugo,[sport,jardinage]).
hobby(tom,[sport,voyages]).
hobby(louis,[sport,jardinage]).
hobby(paul,[sport]).
hobby(jean,[sport,jardinage]).
hobby(antoine,[sport,jardinage]).

incompatible(manon,louis).
incompatible(charlotte,antoine).
incompatible(margaux,hugo).
