% Representing idntifiers and numbers using the predicates - id and numb

% id predicate - denotes if X is a identifier, all atoms are considered as identifiers
id(X):-
	atom(X).


% num(N) - denote if N is a natural number, i.e if N is a integer greater than or equal to 0
num(N):-
	integer(N),
	N>=0.

% Creating predicate for to execute commands.
% execute(S0, C, SN) - here S0 is the initial binding environment, C is the command to execute 
%					   and SN is the finial binding environment caused from executing the command.

% base case execute(S0, skip, S0) - exection of command skip causes not change in binding environment hence SN = S0
execute(S0, skip, S0).

% base case execute(S0, set(I,E), SN) - execution of command set(I,E) updates binding of identifier I with evaluation of expression E to give new binding environment SN
execute(S0, set(I,E), SN):-
	evaluate(S0, E, RES),
	update_binding(S0, I, RES, SN).

% recursive case execute(S0, if(B, C1, _), SN) - if boolean expression B is true then execute command C1
execute(S0, if(B, C1, _), SN):-
	evaluate(S0,B,RES),
	RES=tt,
	execute(S0, C1, SN).

% recursive case execute(S0, if(B, _, C2), SN) - if boolean expression B is false then execute command C2
execute(S0, if(B, _, C2), SN):-
	evaluate(S0,B,RES),
	RES=ff,
	execute(S0, C2, SN).

% recursive case execute(S0, while(B, _), S0) - if boolean expression B is false then final binding same as initial binding.
execute(S0, while(B, _), S0):-
	evaluate(S0, B, RES),
	RES=ff.

% recursive case execute(S0, while(B, _), S0) - if boolean expression B is true then execute command C1 and then do with the new bindings execute while(B, C1)
execute(S0, while(B, C1), SN):-
	evaluate(S0, B, RES),
	RES=tt,
	execute(S0, C1, Sres),
	execute(Sres, while(B, C1), SN).

% recursive case execute(S0, seq(C1, C2), SN) - if boolean expression B is true then execute command C1 and then do with the new bindings execute C2
execute(S0, seq(C1, C2), SN):-
	execute(S0, C1, Sres),
	execute(Sres, C2, SN).

% Creating predicate for to evaluate expressions both arithmetic and boolean.
% evaluate(S0, E, RES) - S0 is the initial binding environment, E is the expression and RES is the result of evaluating the expression using the initial binding environment
evaluate(S0, id(X), RES):-
	search_binding(S0, X, RES).

evaluate(_, num(N), N):-
	num(N).

evaluate(_, ff, ff).

evaluate(_, tt, tt).

evaluate(S0, X + Y, RES):-
	evaluate(S0, X, Xres),
	evaluate(S0, Y, Yres),
	RES is Xres + Yres.

evaluate(S0, X - Y, RES):-
	evaluate(S0, X, Xres),
	evaluate(S0, Y, Yres),
	RES is Xres - Yres.

evaluate(S0, X * Y, RES):-
	evaluate(S0, X, Xres),
	evaluate(S0, Y, Yres),
	RES is Xres * Yres.

evaluate(S0, X / Y, RES):-
	evaluate(S0, X, Xres),
	evaluate(S0, Y, Yres),
	RES is Xres / Yres.

evaluate(S0, X > Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1>Val2.

evaluate(S0, X < Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1<Val2.

evaluate(S0, X >= Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1>=Val2.

evaluate(S0, X =< Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=<Val2.

evaluate(S0, X == Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=:=Val2.

evaluate(S0, X =\= Y, tt):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=\=Val2.

evaluate(S0, X > Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=<Val2.

evaluate(S0, X < Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1>=Val2.

evaluate(S0, X >= Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1<Val2.

evaluate(S0, X =< Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1>Val2.

evaluate(S0, X == Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=\=Val2.

evaluate(S0, X =\= Y, ff):-
	evaluate(S0, X, Val1),
	evaluate(S0, Y, Val2),
	Val1=:=Val2.

% creating predicate to update the binding table
% binding table is a list of the form [X1=Y1, X2=Y2, ...] where Xi is a variable and Yi is its value 
% update_binding(S0, X, Y, SN) - S0 is the initial binding environment, X is the variable to updaye, Y is the updated value of X, SN is the updated binding enviroment.
update_binding([],X,Y,[X=Y]).
update_binding([X=_|S0x], X, Y, [X=Y|S0x]).
update_binding([S0=V|S0x], X, Y, [S0=V|SNx]):-
	S0\=X,
	update_binding(S0x, X, Y, SNx).

% creating predicate to search for value of a variable in the binding table.
% search_binding(S0, X, Y) - S0 is the binding environment, X is the variable, Y is the value of the variable in the binding environment. 
search_binding([X=Y|_], X, Y).
search_binding([_|S0x], X, Y):-
	search_binding(S0x, X, Y).


	
	