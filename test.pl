married(adam, eva).
married(kungen, silvia).
married(fantomen, diana).



%permutation(Xs,Ys) is true if Xs is a permutation of Ys.
permutation([], []).
permutation([X|Xs], Ys) :- select(X, Ys, Ys1), permutation(Xs, Ys1).

select(X, [X|Ys], Ys).
select(X, [Y|Ys], [Y|Zs]) :- select(X, Ys, Zs).
