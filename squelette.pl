:- use_module(library(clpfd)).
[bd].
% ---------------------------------------------------------------------- %
%                                                                        %
%                        PREDICAT PRINCIPAL                              %
%                                                                        %
% ---------------------------------------------------------------------- %

go :- Lconvives = [  manon, alice, juliette, lucie, charlotte, olivia, margaux,
                     jules, hugo, tom, louis, paul, jean, antoine  ],
      table(Lconvives).


table(Lconvives) :-

    length([anne,michel|Lconvives],Nb),
    produire_lassoc([anne,michel|Lconvives],Nb,Lassoc),
/*
    alternance_homme_femme(Lassoc,Nb),
    meme_hobby(Lassoc,Nb),
    pas_epoux_cote_a_cote(Lassoc,Nb),
    pas_incompatibilite(Lassoc,Nb),
    places_differentes(Lassoc),
    label_places(Lassoc),
*/
    sort(2,@<,Lassoc,Lassoc_trie),
    impression_table(Lassoc_trie).



% ---------------------------------------------------------------------- %
%                                                                        %
%                          LISTE ASSOCIATIVE                             %
%
%􏰂 produire_lassoc(Lpers,Lg,Lassoc,Nb),
% crée dans Lassoc une association pers_var(personne,var)
% liant chaque pers. à une variable et dé􏰁nissant le domaine de celle-ci
% comme 1 . . . Nb ;                                                                       %
% ---------------------------------------------------------------------- %

produire_lassoc( [H1,H2|T],N,[HR|TR] ) :-

      N is length(L).


% ---------------------------------------------------------------------- %
%                                                                        %
%                               CONTRAINTES                              %
%                                                                        %
% ---------------------------------------------------------------------- %


% alternance_homme_femme(Lassoc,Nb)
% spéci􏰁fie que des personnes du même genre ne peuvent être assises
% l'une à côté de l'autre
% -----------------------------------------------------------------------%

 alternance_homme_femme(  ) :-  .


% Même hobby
%     spéci􏰁fie que des personnes ne partageant pas un même hobby
%     ne peuvent être l'une à côté de l'autre
% -----------------------------------------------------------------------%

meme_hobby(  ) :-  .


% Pas epoux cote a cote
% ---------------------

pas_epoux_cote_a_cote(  ) :-  .


% Pas d incompatibilite
% ---------------------

pas_incompatibilite(  ) :-  .


% Personnes a des places differentes
% ----------------------------------

 places_differentes(  ) :- .

% ---------------------------------------------------------------------- %
%                                                                        %
%                                LABELING                                %
%                                                                        %
% ---------------------------------------------------------------------- %

label_places(  ) :-  .

% ---------------------------------------------------------------------- %
%                                                                        %
%                                IMPRESSION                              %
%                                                                        %
% ---------------------------------------------------------------------- %

impression_table([]) :- !, nl.
impression_table([pers_var(P,V)|T]) :-
    nl, format('~w ~d ~w ~a',['Place ',V, ' :', P]),
    impression_table(T).
