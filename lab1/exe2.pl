/* Representing the graph as a set of facts using predicate edge

edge(a,b) - depecits that there is an from a to b*/

edge(a,b).
edge(a,c).
edge(b,c).
edge(c,d).
edge(c,e).
edge(d,h).
edge(d,f).
edge(e,f).
edge(e,g).
edge(f,g).

% path(X,Y) - predicate that depicts that there is a path from X to Y
% Base case of path(X,Y) - Path exits between X and Y if there is a node Z such that there is edge from X to Z and from Z to Y
path(X,Y):-
	edge(X,Y).

% Recursive case of path(X,Y) - Path exits between X and Y if there is a node Z such that there is a edge between X to Z and there exist a path from Z to Y
% recusrive call is not given as the first predicate as this can lead to infinite loops during resolution.
path(X,Y):-
	edge(X,Z),
	path(Z,Y).

% path(X,Y, [Z,Zs]) - Depicts that a path exists from X to Y and the list [Z|Zs] depicts the path in between X and Y
% base case path/3 - path(X, Y, [X,Y]) - The path between X and Y is the [X|Y] if there is an edge connecting them
path(X, Y, [X,Y]):-
	edge(X,Y).

% recursive case of path/3 - path(X, Z, [X,T]) - The path between X and Z is the list [X|T] if there is a edge from X to H and there is a path from H to Z have the path in list [T]
path(X, Z, [X|T]):-
	edge(X,H),
	path(H,Z, T).

% npath(X, Y, N) - Depicts that there is a path between X and Y and also that it is of length N.
npath(X, Y, N):-
	path(X, Y, Z),
	length(Z, N).