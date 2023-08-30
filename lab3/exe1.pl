% run(In, String, Out) - Predicate that runs the program give, 
%						 In is the inital binding environment, String is the program to run and Out is the finial binding environment
run(In, String, Out) :-
    scan_swi_prolog(String, Tokens),
    parse(Tokens, AbstStx),
    execute(In, AbstStx, Out).

% parse(InputTokens, Command) - parse is a rule that shows the relationship between InputToken of the program and the commands of the interperter
%								relation described by a DCG with pgm as the starting 
parse(InputTokens, Command):-
	pgm(Command, InputTokens, []).

% pgm(X) - denotes a program, X is a string of commands from the Interpretor
% pgm(X) - can either be a single cmd or a set of commands if X is the command seq.
pgm(X) -->
	cmd(X).

pgm(seq(C1,C2)) -->
	cmd(C1),
	[;],
	pgm(C2).

% cmd(X) - denotes a prgram command, X is a commands of the interpretor
% cmd(X) - can be,
%				[skip] | S is a command skip
%				[I, :=, E] | X is a command set(IE), 
%				[while, B, do, C, od] | X is a command while(B,C)
%				[if, B, then, C1, else, C2, fo] | X is a command if(B,C1,C2)
% where B is boolean experssion, E is a expression, C, C1, C2 can be a prm i.e a command or set of commands
cmd(skip) -->
	[skip].

cmd(set(I,E)) -->
	[id(I)],
	[:=],
	expr(E).

cmd(while(B,C)) -->
	[while],
	bool_expr(B),
	[do],
	pgm(C),
	[od].

cmd(if(B,C1,C2)) -->
	[if],
	bool_expr(B),
	[then],
	pgm(C1),
	[else],
	pgm(C2),
	[fi].

% bool_expr(X) - denotes a program boolean experssion, where X is a boolean expression from the interpretor
% bool_expr(X) - can be,
%					[X, >, Y] | X is X > Y
%					[X, <, Y] | X is X < Y
%					[X, ==, Y] | X is X == Y
% where X, Y are expressions
bool_expr(X > Y) -->
	expr(X),
	[>],
	expr(Y).

bool_expr(X < Y) -->
	expr(X),
	[<],
	expr(Y).

bool_expr(X == Y) -->
	expr(X),
	[==],
	expr(Y).

% expr(X) - denotes a program expression, X is expression of the interpretor
% expr(X) can be,
%				X+Y       | X
%				X-Y       | X
% where X is a expression and Y a factor
expr(X) -->
	factor(X).

expr(X+Y) -->
	factor(X),
	[+],
	expr(Y).

expr(X-Y) -->
	factor(X),
	[-],
	expr(Y).

% factor(X) - denotes a program factor, X is factor of the interpretor
% factor(X) can be,
%				X*Y       | X
% where X is a term and Y a factor
factor(X) -->
	term(X).

factor(X * Y) -->
	term(X),
	[*],
	factor(Y).

% term(X) - denotes a program term, X is term of the interpretor
% factor(X) can be id(X) or num(X)
term(id(X)) -->
	[id(X)].

term(num(X)) -->
	[num(X)].


% Scanner for assignment 3
% TDDD08 Logic Programming
%
% top predicate:
% scan(+String, -Tokens) 
%
% try: scan("x:=3; y:=1; while x>1 do y := y*x; x := x-1 od",Tokens).
%
% NOTE: strings are lists of ASCII codes, i.e.
% "Prolog" = [80,114,111,108,111,103]

% NOTE: Under recent versions of SWI-Prolog (which is used in e.g. Swish) strings use a different
%representation. Uncomment the following line and use scan_swi_prolog instead of scan/2 if you use
%SWI-Prolog (the predicate simply converts a SWI-Prolog string to a list of codes).

scan_swi_prolog(String, Tokens) :- string_codes(String, String1), scan(String1, Tokens).

scan([],[]).
scan([C|Cs],[';'|Ts]) :-
	semicolon(C),!,
	scan(Cs,Ts).
scan([C|Cs],Ts) :-
	space(C),!,
	scan(Cs,Ts).
scan([C|Cs],[num(T)|Ts]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CNum),
	name(T,[C|CNum]),
	scan(Cs1,Ts).
scan([C1,C2|Cs],[T|Ts]) :-
	name(T,[C1,C2]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]):-
	name(T,[C]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]) :-
	letter(C),
	scan_key_or_id(Cs,Cs1,CWord),
	name(Word,[C|CWord]),
	classify(Word,T),
	scan(Cs1,Ts).

% scaning a number
% scan_number(+In, -Out, -Num)
% Num is a string of digits from front of In,
% Out is the remaining string

scan_number([C|Cs],Cs1,[C|CN]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CN).
scan_number(Cs,Cs,[]).

% scaning a keyword or an identifier
% scan_key_or_id(+In, -Out, -Word)
% Word is a string from front of In,
% Out is the remaining string

scan_key_or_id([C|Cs],Cs1,[C|CW]) :-
	(letter(C)
	 ;
	 digit(C)
	),!,
	scan_key_or_id(Cs,Cs1,CW).
scan_key_or_id(Cs,Cs,[]).

% distinguishing keywords from identifiers

classify(W,T) :-
	keyword(W),!,
	T = W.
classify(W,id(W)).


digit(C) :-
	C >= "0", C =< "9".


letter(C) :-
	C >= "a", C =< "z"
	;
	C >= "A", C =< "Z".


semicolon(59).


operator('*').
operator('+').
operator('/').
operator('-').
operator('>').
operator('<').
operator('=').
operator('=<').
operator('>=').
operator(':=').


space(32).


keyword(skip).
keyword(if).
keyword(then).
keyword(else).
keyword(fi).
keyword(while).
keyword(do).
keyword(od).
keyword(true).
keyword(false).


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


	
	