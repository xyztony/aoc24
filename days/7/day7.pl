% :- use_module('../../utils').
:- use_module(library(between)).
:- use_module(library(builtins)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pio)).

equations([]) --> eol.
equations([Eq|Eqs]) --> equation(Eq), equations(Eqs).

equation(eq(Goal, Ns)) --> number(Goal), ":", numbers(Ns).

numbers([]) --> eol.
numbers([N|Ns]) --> " ", number(N), numbers(Ns).

is_valid_solution(eq(Goal, Ns)) :-
        reverse(Ns, RNs),
        sol(eq(Goal, RNs)).

sol(eq(Goal, [N])) :- Goal #= N.
sol(eq(Goal, [N|Ns])) :-
        sol(eq(SubGoal, Ns)),
        (   Goal #= N + SubGoal
        ;   Goal #= N * SubGoal).

sum_goals(Eqs, Sum) :-
    findall(G, (member(eq(G,_), Eqs)), Goals),
    sum_list(Goals, Sum).

part1(F, Sum) :-
        phrase_from_file(equations(Eqs), F),
        setof(Eq, (member(Eq, Eqs), has_solution(Eq)), ValidEqs),
        sum_goals(ValidEqs, Sum).

?- part1("./part1.txt", Sum).
