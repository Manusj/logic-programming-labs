/* Data given has been converted into facts by using a predicate
* beautiful(X) - indicates that a preson is beautiful
* rich(X) - indicates that a person is rich
* strong(X) - indicates that a person is strong
* kind(X) - indicates that a peson is kind
* woman(X) - indicates that a person is a womain
* man(X) - indicates that a person is a man
* 
* ulrika is considered as a woman as she likes men as per the given facts
* Anne is considered as  a woman based on her name, no specific infomation given in the facts
* Peter, bosse are considered as men based on their name, no specific infomation given in the facts	
* Nisse is considered as men as he likes woman
*/

man(peter).
man(bosse).
man(nisse).
woman(ulrika).
woman(anne).
beautiful(ulrika).
beautiful(nisse).
beautiful(peter).
rich(nisse).
rich(anne).
strong(anne).
strong(peter).
strong(bosse).
kind(bosse).


/* Rule have been created based on the statement given to define the predicates like and happy
* happy(X) - denotes that a person is happy
* like(X,Y) - denotes that a person X likes person Y
*/ 

% Based on statement 6 all men likes beautiful women
like(X,Y):- 
	man(X), 
	woman(Y), 
	beautiful(Y).

% Based on statement 10 Nisse likes all women who like him
like(nisse, Y):- 
	woman(Y), 
	like(Y, nisse).

% Based on statement 11 peter likes everypne who is kind
like(peter, Y):- 
	kind(Y).

% Based on statement 12 ulrika likes men who are either rich and kind, or beautiful and strong.
% Since this statment contains an or case, it has been modeled as two rules.
like(ulrika, Y):- 
	man(Y), 
	rich(Y), 
	kind(Y), 
	like(Y, ulrika).

like(ulrika, Y):- 
	man(Y), 
	beautiful(Y), 
	strong(Y), 
	like(Y, ulrika).

% Based on statement 7 all rich people are happy
happy(X):- 
	rich(X).

% Based on statement 8 is a man is happy when a woman who he likes, likes him back
happy(X):- 
	man(X), 
	woman(Y), 
	like(X,Y), 
	like(Y,X).

% Based on statement 9 is a woman is happy when a man who she likes, likes her back
happy(Y):-  
	woman(Y), 
	man(X), 
	like(Y,X),
	like(X,Y).