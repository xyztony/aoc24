:- use_module('../../utils').
:- use_module(library(assoc)).
:- use_module(library(builtins)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(iso_ext)).
:- use_module(library(pio)).


:- dynamic(page_order/2).

order --> number(X), "|", number(Y), "\n", { assertz(page_order(X, Y)) }, order.
order, "\n" --> "\n".

manual([X]) --> number(X), eol.
manual([X|Xs]) --> number(X), ",", manual(Xs).

manuals([]) --> eol.
manuals([X|Xs]) --> manual(X), manuals(Xs).

input(X) --> order, "\n", manuals(X).

pairs_from_list(Ls, X, Y) :-
        append(_, [X|Xs], Ls),
        select(Y, Xs, _).

is_sorted(L) :-
    forall(pairs_from_list(L,X,Y), \+ page_order(Y, X)).

middle(L, M) :-
    length(L, N),
    Mid is N // 2,
    nth0(Mid, L, M).

is_sorted_then_increment(L, Acc0, Acc) :-
    is_sorted(L), middle(L,Mid), Acc #= Acc0 + Mid ; Acc #= Acc0.

part1(F, Sol) :-
    phrase_from_file(input(X), F), foldl(is_sorted_then_increment, X, 0, Sol).

?- part1("./part1.txt", S).

/* -----------------------------------------------------------------------
   modified from https://www.metalevel.at/misc/sorting.pl
----------------------------------------------------------------------- */
mergesort(Ls0, Ls) :-
        length(Ls0, L),
        zcompare(C, L, 1),
        halving(C, L, Ls0, Ls).

halving(<, _, Ls, Ls).
halving(=, _, Ls, Ls).
halving(>, L, Ls0, Ls) :-
        Half #= L // 2,
        length(Lefts0, Half),
        append(Lefts0, Rights0, Ls0),
        mergesort(Lefts0, Lefts),
        mergesort(Rights0, Rights),
        merge(Lefts, Rights, Ls).

% this was just modified to sort by the page ordering fn `page_order`
merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], Ms) :-
        (   page_order(X,Y) ->
            Ms = [X|Rs],
            merge(Xs, [Y|Ys], Rs)
        ;   Ms = [Y|Rs],
            merge([X|Xs], Ys, Rs)
        ).
/* -------------------------------------------------------------------- */

% find incorrectly sorted orders, sort, and then update accumulator
sort_and_increment(L, Acc0, Acc) :-
        (   is_sorted(L)
        ->  Acc #= Acc0
        ;   mergesort(L, L1),
            middle(L1, Mid),
            Acc #= Acc0 + Mid
        ).

part2(F, Sol) :-
    phrase_from_file(input(X), F), foldl(sort_and_increment, X, 0, Sol).


?- part2("./part1.txt", S).
