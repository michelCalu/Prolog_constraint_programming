% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space
:- style_check(- singleton).
:- use_module(library(clpfd)).

go(JL):- End=20,
    problem(Data),
    schedule(Data, End, JL).

problem([task(j1,3,[],m1),
 task(j2,8,[],m1),
 task(j3,8,[j4,j5],m1),
 task(j4,6,[],m2),
 task(j5,3,[j1],m2),
 task(j6,4,[j1],m2)]
).



schedule(Data,End,Jobs) :-
	makejobs(Data, Jobs, End),
	precedences(Data, Jobs),
	machines(Data, Jobs),
	labeltasks(Jobs).

labeltasks([]).
labeltasks([V|Vs]) :- indomain(V), labeltasks(Vs).


makejobs([],[],_).
makejobs([task(N,D,_,_)|Ts],[job(N,D,TS)|Js], End) :-
	TS in 0..1000,
    TS + D #=< End,
	makejobs(Ts,Js,End).
getjob(JL,N,D,TS) :- member(job(N,D,TS),JL).

precedences([],_).
precedences([task(N,_,P,_)|Ts],JL) :-
	getjob(JL,N,_,TS),
	prectask(P,TS,JL),
	precedences(Ts,JL).

prectask([],_,_).
prectask([N|Ns],TS0,JL):-
	getjob(JL,N,D,TS1),
	TS1 + D #=< TS0,
	prectask(Ns,TS0,JL).

machines([],_).
machines([task(N,_,_,M)|Ts],JL) :-
	getjob(JL,N,D,TS),
	machtask(Ts,M,D,TS,JL),
	machines(Ts,JL).
machtask([],_,_,_,_).
machtask([task(N,_,_,M1)|Ts],M0,D0,TS0,JL):-
	(M1 = M0 -> getjob(JL,N,D1,TS1),
			 exclude(D0,TS0,D1,TS1)
	;          true),
	machtask(Ts,M0,D0,TS0,JL).


exclude(_,TS0,D1,TS1) :- D1 + TS1#=<TS0.
exclude(D0,TS0,_,TS1) :- D0 + TS0#=<TS1.
