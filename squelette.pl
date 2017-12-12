:- style_check(- singleton).
:- use_module(library(clpfd)).
:- use_module(library(statistics)).

% ---------------------------------------------------------------------- %
%                                                                        %
%                        PREDICAT PRINCIPAL                              %
%                                                                        %
% ---------------------------------------------------------------------- %
noms([  manon, alice, juliette, lucie, charlotte,
        olivia, margaux,jules, hugo, tom, louis, paul, jean, antoine  ]).


go :- noms(Lconvives),
      diner(Lconvives).

diner(Lconvives) :-

    length([anne,michel|Lconvives],Nb),
    produire_lassoc([anne,michel|Lconvives],Lassoc, Nb),
    produire_ldonnees([anne,michel|Lconvives],Data),
    alternance_homme_femme(Lassoc,Nb),
    pas_epoux_cote_a_cote(Lassoc,Data,Nb),
    meme_hobby(Lassoc, Nb, Data),
    pas_incompatibilite(Data, Lassoc),
    label_places(Lassoc,Nb),
    sort(2,@<,Lassoc,Lassoc_trie),
    writeln(Lassoc_trie),
    impression_table(Lassoc_trie).


% ---------------------------------------------------------------------- %
%                          LISTE ASSOCIATIVE                             %
% ---------------------------------------------------------------------- %
produire_lassoc( [], _, [] ).
produire_lassoc(List,Lassoc, Nb):-
    produire_persvar(List, Lassoc),
    term_variables(Lassoc,ListVar),
    ListVar ins 1..Nb,
    mme_mr(ListVar,Nb).

/**************************************************************************/
% produire_persvar(Liste_de_convives, Liste_de_pers_Var)

produire_persvar([],[]).
produire_persvar([H|T],[pers_var(H,Var)|Rest]):-
    produire_persvar(T,Rest).

/**************************************************************************/
% mme_mr( pers_var(Madame), pers_var(Monsieur)
%

mme_mr([Mme,Mr|_],Nb):-
    Mme in 1..1,
    Mid is Nb/2,
    Mr in Mid..Mid.

getvar(PersVar,Var):-
    PersVar=pers_var(_,Var).

getPers_var(Pers, Var, List):-
    member(pers_var(Pers, Var), List).


produire_ldonnees([], []).
produire_ldonnees([H|T], [p(H,S,Part,Hob,Incomp)|Rest]):-
    sexe(H,S),
    partenaire(H,Part),
    hobby(H,Hob),
    incompatibilites(H,Incomp),
    produire_ldonnees(T, Rest).

incompatibilites(X,[E]):-
    incompatible(X,E).
incompatibilites(X,[]):-
   not(incompatible(X,_)).
partenaire(X,Partenaire):-
    epoux(X,Partenaire),!.
partenaire(X,Partenaire):-
    epoux(Partenaire,X).

% ---------------------------------------------------------------------- %
%                                                                        %
%                               CONTRAINTES                              %
%                                                                        %
% ---------------------------------------------------------------------- %

% Alternance homme-femme
% ----------------------

alternance_homme_femme( [],_ ).
alternance_homme_femme( [pers_var(P,V)|Rest],Nb ) :-
    (   sexe(P,m) *-> true,
         V mod 2 #= 0
    ;   true,   V mod 2 #= 1
    ),
    alternance_homme_femme( Rest,Nb ).


% Pas epoux cote a cote
% ---------------------
pas_epoux_cote_a_cote(_,[],_ ).
pas_epoux_cote_a_cote(Lassoc, [p(Name1,_,Epoux_1,_,_)|T], Nb ):-
    getPers_var(Epoux_1, VarEpoux1, Lassoc),
    getPers_var(Name1,VarName1, Lassoc),
    abs(VarEpoux1 mod 16 - VarName1 mod 16)#>1,
    pas_epoux_cote_a_cote(Lassoc, T, Nb).


% Meme hobby
% ----------

meme_hobby( [], _, _ ).
meme_hobby( [pers_var(Name1, Var1)|T], Nb, Data ) :-
     member(p(Name1,_,_,Hobby_1,_), Data),
     l_incompatibles(Hobby_1, Data, Incompatibles),
     eliminer_hobby_incomp(Incompatibles, Var1, Lassoc),
     meme_hobby(T, Nb, Data).

eliminer_hobby_incomp([], _, _).
eliminer_hobby_incomp([H|T], VarName, Lassoc):-
    getPers_var(H, Var2, Lassoc),
    abs(VarName mod 16 - Var2 mod 16)#>1,
    eliminer_hobby_incomp(T, VarName, Lassoc).



l_incompatibles(Hobby_1, [], []).
l_incompatibles( Hobby_1, [p(_,_,_,Lhobbies,_)|T], Rest):-
    intersection(Hobby_1, Lhobbies, X),
    length(X,N), N#>0,
    l_incompatibles(Hobby_1, T, Rest).

l_incompatibles( Hobby_1, [p(Name,_,_,Lhobbies,_)|T], [Name|Rest]):-
    intersection(Hobby_1, Lhobbies, []),
    l_incompatibles(Hobby_1, T, Rest).




% Pas d incompatibilite
% ---------------------

pas_incompatibilite([], Lassoc ).

pas_incompatibilite([p(Name1,_,_,_,[])|T], Lassoc ):-
    pas_incompatibilite(T,Lassoc).

pas_incompatibilite([p(Name1,_,_,_,Incompatibles)|T], Lassoc ):-
    length(Incompatibles,L), L#>0,
    getPers_var(Name1, Var1, Lassoc),
    eliminer_ennemis(Incompatibles, Var1, Lassoc),
    pas_incompatibilite(T, Lassoc).

eliminer_ennemis([], _, Lassoc).
eliminer_ennemis([E|Ennemis], Var, Lassoc):-
    getPers_var(E, VarE, Lassoc),
    abs(Var mod 16 - VarE mod 16 )#>1,
    eliminer_ennemis(Ennemis, Var, Lassoc).



% ---------------------------------------------------------------------- %
%                                                                        %
%                                LABELING                                %
%                                                                        %
% ---------------------------------------------------------------------- %

label_places(Lassoc,Nb):-
    get_variables(Lassoc,Labels),
    all_different(Labels),
    label(Labels).

get_variables([], []).
get_variables([H|T], [VarH|Rest]):-
    getvar(H,VarH),
    get_variables(T, Rest).

% ---------------------------------------------------------------------- %
%                                                                        %
%                                IMPRESSION                              %
%                                                                        %
% ---------------------------------------------------------------------- %

impression_table([]) :- !, nl.
impression_table([pers_var(P,V)|T]) :-
    nl, format('~w ~d ~w ~a',['Place ',V, ' :', P]),
    impression_table(T).

% ---------------------------------------------------------------------- %
%                                                                        %
%                            BASE DE DONNEES                             %
%                                                                        %
% ---------------------------------------------------------------------- %

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
