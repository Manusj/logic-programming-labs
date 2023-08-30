% union(A,B,AUB) - Rule to create union of two sets, sets are represented by an ordered list,
%				   set AUB is the union of set A and set B.

% base case union([],[],[]) - union of two empty sets is an empty set
% base case union(X,[],X), union([],X,X) - union of an empty set and a non empty set is the non empty set
union([],[],[]).
union([],X,X).
union(X,[],X).

% recursive case union([X|Xs],[Y|Yx],[X|Z]) - union of [X|Xs] and [Y|Yx] is [X|Z] if X<Y and Z is the union of Xs & [Y|Yx].
union([X|Xs],[Y|Yx],[X|Z]):-
	X@<Y,
	union(Xs,[Y|Yx],Z).

% recursive case union([X|Xs],[Y|Yx],[Y|Z]) - union of [X|Xs] and [Y|Yx] is [Y|Z] if X>Y and Z is the union of [X|Xs] & Yx.
union([X|Xs],[Y|Yx],[Y|Z]):-
	X@>Y,
	union([X|Xs],Yx,Z).

% recursive case union([X|Xs],[Y|Yx],[Y|Z]) - union of [X|Xs] and [Y|Yx] is [X|Z] if X=Y and Z is the union of Xs & Yx.
union([X|Xs],[X|Yx],[X|Z]):-
	union(Xs,Yx,Z).

% intersection(A,B,AIB) - Rule to create the intersection of two sets,
%						  set AIB is the intersection of set A and set B

% base case intersection([],[],[]), intersection(_,[],[]) ,intersection([],_,[]) - the intersection of an empty set with any set is an empty set.
intersection([],[],[]).
intersection(_,[],[]).
intersection([],_,[]).

% recursive case intersection([X|Xs],[X|Ys],[X|Z]) - [X|Z] is the intersection of [X|Xs] and [X|Ys] if the intersection of Xs and Ys is Z
intersection([X|Xs],[X|Ys],[X|Z]):-
	intersection(Xs,Ys,Z).

% recursive case intersection([X|Xs],[Y|Ys],Z) - Z is the intersection of [X|Xs] and [Y|Ys] if X<Y and the intersection of Xs and [Y|Ys] is Z
intersection([X|Xs],[Y|Ys],Z):-
	X@<Y,
	intersection(Xs,[Y|Ys],Z).

% recursive case intersection([X|Xs],[Y|Ys],Z) - Z is the intersection of [X|Xs] and [Y|Ys] if X>Y and the intersection of [X|Xs] and Ys is Z
intersection([X|Xs],[Y|Ys],Z):-
	X@>Y,
	intersection([X|Xs],Ys,Z).

% subset(A,B) - rule to find subset. then B is a subset of set A.
% base case subset([],[]) - the empty set is a subset of itself
subset([],[]).

% recursive case subset([X|Xs],[X|Ys]) - if Ys is a subset of Xs then [X|Ys] is a subset of [X|Xs]
subset([X|Xs],[X|Ys]):-
	subset(Xs,Ys).

% recursive case subset([_|Xs],Ys) - if Ys is a subset of Xs then Ys is a subset of [_|Xs]
subset([_|Xs],Ys):-
	subset(Xs,Ys).

% powerset(X,Px) - Px is the powerset if set X. powerset is the set of all subsets.
%				 - Px is a set of all subset, so Px is a sorted list of subsets
powerset(X,Px):-
	findall(Y,subset(X,Y),R),
	sort(R,Px).
