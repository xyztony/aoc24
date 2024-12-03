:- use_module('../../utils').

:- use_module(library(between)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(pio)).

report([X]) --> number(X), eol.
report([X|Xs]) --> number(X), " ", report(Xs).

input([]) --> call(eos).
input([L|Ls]) --> report(L), input(Ls).

mono_dec([_]).
mono_dec([X, Y|T]) :-
        D #= X - Y, between(1, 3, D), mono_dec([Y|T]).

mono_inc([_]).
mono_inc([X, Y|T]) :-
        D #= Y - X, between(1, 3, D), mono_inc([Y|T]).

/* -----------------------------------------------------------------------
   ?- include(mono_dec, [[7,6,4,2,1], [1,2,7,8,9], [9,7,6,2,1], [8,6,4,4,1], [1,3,6,7,9]], Ls).
   ?- include(mono_inc, [[7,6,4,2,1], [1,2,7,8,9], [9,7,6,2,1], [8,6,4,4,1], [1,3,6,7,9]], Ls).
----------------------------------------------------------------------- */


% part 1
safe_mono(L) :-
        mono_inc(L) ; mono_dec(L).

safe(Ls, ValidLs) :-
        findall(L, (member(L, Ls), safe_mono(L)), ValidLs).

count_part1(File, Count) :-
        phrase_from_file(input(Xs), File),
        safe(Xs, Valid),
        length(Valid, Count).

% ?- count_part1("./day2.txt", C).

% part 2
safe_with_dampener(L) :-
        safe_mono(L); select(_, L, Ls1), safe_mono(Ls1).

safe_dampener(Ls, ValidLs) :-
        include(safe_with_dampener, Ls, ValidLs).

count_mono_dampener(Ls, Count) :-
        safe_dampener(Ls, Valid),
        length(Valid, Count).

count_part2(File, Count) :-
         phrase_from_file(input(Xs), File),
         safe_dampener(Xs, ValidLs),
         length(ValidLs, Count).

?- count_part2("./day2.txt", C).
%@    error(existence_error(procedure,count_part2/2),count_part2/2).
















