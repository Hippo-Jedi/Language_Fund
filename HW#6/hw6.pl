
# Exercise 1

redefine_system_predicate(when).

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

# Part A

schedule(S,P,T) :- enroll(S,C), when(C,T), where(C,P).

# Part B

usage(P,T) :- where(C,P), when(C,T).

# Part C

conflict(C1,C2) :- when(C1,T), when(C2,T), where(C1,P), where(C2,P), C1 \= C2.

# Part D

meet(S1,S2) :- enroll(S1,C), enroll(S2,C), S1 \= S2;
               enroll(S1,C1), enroll(S2,C2), when(C1,T1), when(C2,T2), succ(T1,T2), S1 \= S2.


# Exercise 2

# Part A

rdup([],[]).
rdup([X|Xs], Out) :- member(X, Xs), !, rdup(Xs, Out).
rdup([X|Xs], [X|Out]) :- rdup(Xs, Out).

# Part B

flat([],[]).
flat([X|Xs],R) :- flat(X,Ra), flat(Xs,Rb), !, append(Ra,Rb,R).
flat(X,R) :- append([X],[],R).

# Part C

project(_,[],[]) :- !.
project([1|I], [L|Ls], R) :- !, append([L],Rs,R), subOneList(I,Is), project(Is, Ls, Rs).
project(I,[_|Ls],R) :- subOneList(I,Is), project(Is, Ls, R).

subOneList(Xs, Ys) :- maplist(decElem, Xs, Ys).
decElem(X, Y) :- Y is X-1.