:- style_check(- singleton).
:- use_module(library(clpfd)).
:- use_module(library(statistics)).

% ---------------------------------------------------------------------- %
%                                                                        %
%                        PREDICAT PRINCIPAL                              %
%                                                                        %
% ---------------------------------------------------------------------- %
noms([manon, alice, juliette, lucie, charlotte,
      olivia, margaux,jules, hugo, tom, louis, paul, jean, antoine
      ,pascale, daniel, brice, doro, stef, manu,alexia,ponpon,alain,laeticia,bonnie,clyde
     ]).

go :-                         %lancement
      noms(Lconvives),
      time(diner(Lconvives)).

diner(Lconvives) :-
    length([anne,michel|Lconvives],Nb),
    produire_lassoc([anne,michel|Lconvives],Lassoc, Nb),
    produire_ldonnees([anne,michel|Lconvives],Data),
    alternance_homme_femme(Lassoc,Nb),
    pas_epoux_cote_a_cote(Lassoc,Data, Nb),
    pas_incompatibilite(Data, Lassoc, Nb),
    meme_hobby(Lassoc, Data, Nb),
    label_places(Lassoc),
    sort(2,@<,Lassoc,Lassoc_trie),
    impression_table(Lassoc_trie),
    write('Strategy:[ '), strat(S),write(S),writeln(']').

% ---------------------------------------------------------------------- %
%                          LISTE ASSOCIATIVE                             %
% ---------------------------------------------------------------------- %
/**************************************************************************/
% produire_lassoc(Liste_invités, Lassoc, Longeur_liste)
%     in: Liste d'invités
%     out: Liste de (pervar(nom1,Variable1),(pervar(nom2,Variable2),...,(pervar(nomNb,VariableNb)
% produire_persvar(Liste_de_convives, Liste_de_pers_Var)
%     in: liste d'invités
%     out: liste de pers_var avec domaines de Anne et Michel fixés à 1..1 et Nb/2..Nb/2+1
%
produire_lassoc( [], _, [] ).
produire_lassoc(List,Lassoc, Nb):-
    produire_persvar(List, Lassoc),
    term_variables(Lassoc,ListVar),
    ListVar ins 1..Nb,
    mme_mr(ListVar,Nb).


produire_persvar([],[]).
produire_persvar([H|T],[pers_var(H,_)|Rest]):-
    produire_persvar(T,Rest).

getvar(PersVar,Var):-
        PersVar=pers_var(_,Var).

getPers_var(Pers, Var, List):-
        member(pers_var(Pers, Var), List).

/**************************************************************************/
% mme_mr( pers_var(Madame), pers_var(Monsieur))
%     in: pers_var(Madame,VarMadame), pers_var(Monsieur,VarMonsieur)
%     out: domaine VarMadame={1}, domaine VarMonsieur={Nb/2} ssi Nb/2 pair, VarMonsieur={Nb/2+1} sinon
%
mme_mr([Mme,Mr|_],Nb):-
    Mid is Nb/2,
     (   Mid mod 2 =:= 0 *-> true,
         Mme in 1..1, Mr in Mid..Mid
    ;   true,
         Mme in 1..1, Mid2 is Mid+1, Mr in Mid2..Mid2
    ).

/**************************************************************************/
% produire_ldonnees(Liste_invités, Liste_de_donnée)
%     in: liste des invités
%     out: agrège par invité (Nom,Sexe,Partenaire,[hobbies], [incompatibilités])
%
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

/**************************************************************************/
% alternance_homme_femme( Lassoc, Longueur)
%     in: liste des invités, Longueur
%     out: domaine femmes={x|x mod 2 = 1}, domaine hommes={x|x mod 2 = 0}

alternance_homme_femme( [], _).
alternance_homme_femme( [pers_var(P,V)|Rest],Nb ) :-
    (   sexe(P,m) *-> true,
         V mod 2 #= 0
    ;   true,   V mod 2 #= 1
    ),
    alternance_homme_femme(Rest,Nb).


/**************************************************************************/
% pas_epoux_cote_a_cote( Lassoc, Longueur)
%     in: liste des invités
%     out: Lassoc tq: forall pers_var(nom1, Var1), pers_var(nom2, Var2), epoux(nom1,nom2) ==> abs(Var1-Var2)>1
%
pas_epoux_cote_a_cote(_, [], _).
pas_epoux_cote_a_cote(Lassoc, [p(Name1,_,Epoux_1,_,_)|T], Nb ):-
    getPers_var(Epoux_1, VarEpoux1, Lassoc),
    getPers_var(Name1,VarName1, Lassoc),
    abs(VarEpoux1 mod Nb - VarName1 mod Nb)#>1,
    pas_epoux_cote_a_cote(Lassoc, T, Nb).


/**************************************************************************/
% meme_hobby( [Lassoc], [Liste_Data], Longueur)
%     in: Lassoc, Data, Longueur
%     out: Lassoc tq: forall pers_var(nom1, Var1), pers_var(nom2, Var2); (abs(Var1-Var2)=1) ==> exists(hobby(nom1,X)AND hobby(nom2,X))
%
meme_hobby( [], _, Nb).
meme_hobby( [pers_var(Name1, Var1)|T], Data, Nb) :-
     member(p(Name1,_,_,Hobby_1,_), Data),
     list_incompatibles(Hobby_1, Data, Incompatibles),
     eliminer_hobby_incomp(Incompatibles, Var1, _, Nb),
     meme_hobby(T, Data, Nb).

list_incompatibles(_, [], []).
list_incompatibles( Hobby_1, [p(_,_,_,Lhobbies,_)|T], Rest):-
   intersection(Hobby_1, Lhobbies, X),
   length(X,N), N#>0,
   list_incompatibles(Hobby_1, T, Rest).
list_incompatibles( Hobby_1, [p(Name,_,_,Lhobbies,_)|T], [Name|Rest]):-
   intersection(Hobby_1, Lhobbies, []),
   list_incompatibles(Hobby_1, T, Rest).


eliminer_hobby_incomp([], _, _,Nb).
eliminer_hobby_incomp([H|T], VarName, Lassoc, Nb):-
    getPers_var(H, Var2, Lassoc),
    abs(VarName mod Nb - Var2 mod Nb)#>1,
    eliminer_hobby_incomp(T, VarName, Lassoc, Nb).



/**************************************************************************/
% pas_incompatibilite( [Liste_Data], [Lassoc], Longueur)
%     in: Data, Lassoc, Longueur
%     out: Lassoc tq: forall pers_var(nom1, Var1), pers_var(nom2, Var2) in Lassoc; (abs(Var1-Var2)=1) ==>
%                       not exists(incompatibilite(nom1,nom2) OR (incompatibilite(nom2,nom1))
%
pas_incompatibilite([], _, Nb ).

% Incompatiblités d'une personne=[]
pas_incompatibilite([p(_,_,_,_,[])|T], Lassoc, Nb ):-
    pas_incompatibilite(T,Lassoc, Nb).

% Incompatiblités d'une personne=[x]
pas_incompatibilite([p(Name1,_,_,_,Incompatibles)|T], Lassoc, Nb ):-
    length(Incompatibles,L), L#>0,
    getPers_var(Name1, Var1, Lassoc),
    eliminer_ennemis(Incompatibles, Var1, Lassoc, Nb),
    pas_incompatibilite(T, Lassoc, Nb).

eliminer_ennemis([], _, _, Nb).
eliminer_ennemis([E|Ennemis], Var, Lassoc, Nb):-
    getPers_var(E, VarE, Lassoc),
    abs(Var mod Nb - VarE mod Nb )#>1,
    eliminer_ennemis(Ennemis, Var, Lassoc, Nb).



% ---------------------------------------------------------------------- %
%                                                                        %
%                                LABELING                                %
%                                                                        %
% ---------------------------------------------------------------------- %

/**************************************************************************/
% label_places( [Lassoc])
%     in: Lassoc
%     out: Lassoc avec labeling t.q pers_var(a,V1),pers_var(a,V2),...pers_var(a,VNb), avec V1,V2,...VNb in 1..Nb
%           AND not exists( pers_var(a,Vx),pers_var(a,Vy) tq a=/=b AND Vx = Vy,
%           AND strat(x): x in {min, max, ff, ffc, leftmost,...}
%
strat(down).                  %stratégie de labeling à appliquer
label_places(Lassoc):-
    get_variables(Lassoc,Labels),
    all_different(Labels),
    strat(Strat),
    labeling([Strat],Labels).

% label variables
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
%Tests
epoux(pascale,daniel).
epoux(manu,stef).
epoux(brice,doro).
epoux(alexia,ponpon).
epoux(alain,laeticia).
epoux(bonnie,clyde).

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
%Tests----------------------------
sexe(pascale,f).
sexe(stef,f).
sexe(doro,f).
sexe(daniel,m).
sexe(brice,m).
sexe(manu,m).
sexe(alexia,f).
sexe(laeticia,f).
sexe(bonnie,f).
sexe(alain,m).
sexe(ponpon,m).
sexe(clyde,m).

hobby(anne,[sport,jardinage,voyages]).
hobby(manon,[jardinage,voyages]).
hobby(alice,[jardinage,voyages]).
hobby(juliette,[sport,voyages]).
hobby(lucie,[jardinage]).
hobby(charlotte,[jardinage]).
hobby(olivia,[sport,jardinage]).
hobby(margaux,[sport]).
%tests------------------------------------
%contraintes supplémentaires
hobby(pascale,[moto, cuisine, lecture]).
hobby(daniel,[football,tennis,curling]).
hobby(stef,[cuisine,curling,voyages]).
hobby(manu,[voile,voyages]).
hobby(brice,[sport,moto,voyages]).
hobby(doro,[cuisine,football]).
hobby(alexia,[voyages]).
hobby(ponpon,[sport,voyages]).
hobby(alain,[sport,jardinage,voyages]).
hobby(laeticia,[jardinage,voyages]).
hobby(bonnie,[sport,jardinage]).
hobby(clyde,[sport,voyages]).


hobby(michel,[sport,jardinage,voyages]).
hobby(jules,[jardinage]).
hobby(hugo,[jardinage]).
hobby(tom,[sport,voyages]).
hobby(louis,[jardinage]).
hobby(paul,[sport]).
hobby(jean,[jardinage]).
hobby(antoine,[sport,jardinage]).

incompatible(manon,louis).
incompatible(charlotte,antoine).
incompatible(margaux,hugo).

incompatible(alexia,louis).
incompatible(charlotte,michel).
incompatible(pascale,hugo).
incompatible(laeticia,louis).
incompatible(charlotte,daniel).
incompatible(margaux,alain).
incompatible(manon,tom).
incompatible(bonnie,brice).
incompatible(pascale,hugo).

incompatible(anne,michel).
incompatible(anne,jules).
incompatible(anne,hugo).
incompatible(anne,tom).
incompatible(anne,louis).
incompatible(anne,antoine).
incompatible(anne,jean).

incompatible(doro,michel).
incompatible(doro,jules).
incompatible(doro,hugo).
incompatible(doro,tom).
incompatible(doro,louis).
incompatible(doro,antoine).
incompatible(doro,jean).
