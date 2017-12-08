% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space
:- style_check(- singleton).
:- use_module(library(clpfd)).

% ---------------------------------------------------------------------- %
%                                                                        %
%                        PREDICAT PRINCIPAL                              %
%                                                                        %
% ---------------------------------------------------------------------- %
testNoms([  manon, alice, juliette, lucie, charlotte, olivia, margaux,
                     jules, hugo, tom, louis, paul, jean, antoine  ]).

go :- Lconvives = [  manon, alice,
              %    juliette, lucie,
              %    charlotte, olivia, margaux, jules, hugo, tom, louis, paul,
                     jean, antoine],
      table(Lconvives).

table(Lconvives,Lassoc) :-

    length([anne,michel|Lconvives],Nb),
    produire_lassoc([anne,michel|Lconvives],Nb,Lassoc),
 %   alternance_homme_femme(Lassoc,Nb).
/*
    meme_hobby(Lassoc,Nb),
    pas_epoux_cote_a_cote(Lassoc,Nb),
    pas_incompatibilite(Lassoc,Nb),
    places_differentes(Lassoc),
    sort(2,@<,Lassoc,Lassoc_trie),
    impression_table(Lassoc_trie).
*/
    label_places(Lassoc),
    sort(2,@<,Lassoc,Lassoc_trie),
    writeln(Lassoc_trie),
    impression_table(Lassoc_trie).


% ---------------------------------------------------------------------- %
%                          LISTE ASSOCIATIVE                             %
% ---------------------------------------------------------------------- %
produire_lassoc( [], _, [] ).
produire_lassoc( [H|T], Nb, [pers_var(H,Var)|Reste] ):-
    set_domain(Var,T,Nb),
    produire_lassoc(T,Nb,Reste).

set_domain(H,T,Nb) :-
    (   length(T,X), C is Nb-2, X#>=C *-> true,             %goal: if Tail>=14
         Mid is Nb/2,
         H in 1..1 \/ Mid..Mid                              %if True: Domain {1..8}
    ;   true,  Mid is Nb/2, Mid_1 is Mid-1, MidPlus1 is Mid+1,
         H in 2..Mid_1 \/ MidPlus1..Nb                      %else: Domain {2..16}\{8}
    ).

getAssoc(List,Nom,Var):- member( pers_var(Nom,Var) , List).






% ---------------------------------------------------------------------- %
%                                                                        %
%                               CONTRAINTES                              %
%                                                                        %
% ---------------------------------------------------------------------- %

% Alternance homme-femme
% ----------------------
/*
alternance_homme_femme( [persvar(Pers,Var)|Rest],Nb ) :-
    oddEven(Pers,Var).

oddEven(Pers,Var) :-
    (   sexe(Pers,f)*-> true,                         %goal
         1 #= Var mod 2                               %if True
    ;   true,
        0 #= Var mod 2                                %else
    ).
*/





% Meme hobby
% ----------

meme_hobby( ) .




% Pas epoux cote a cote
% ---------------------

pas_epoux_cote_a_cote(  ) .




% Pas d incompatibilite
% ---------------------

pas_incompatibilite(  ) .




% Personnes a des places differentes
% ----------------------------------

places_differentes(  ) .





% ---------------------------------------------------------------------- %
%                                                                        %
%                                LABELING                                %
%                                                                        %
% ---------------------------------------------------------------------- %


label_places(Lassoc):-
    makelist(Lassoc,List),
    all_different(List),
    label(List),
    makenew(Lassoc,List).

makelist([], []) .
makelist( [pers_var(P,V)|T], [V|Rest] ):-
    makelist(T,Rest).

makenew([], []).
makenew([pers_var(_,V)|T], [V|Rest]) :-
    makenew(T,Rest).




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
