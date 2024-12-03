:- use_module(library(between)).
:- use_module(library(builtins)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

% from https://www.metalevel.at/prolog/dcg
eol --> ("\n"; call(eos)).
eos([], []).

number(X) --> number([], X).
number(X, NextX) --> [C], { char_type(C, numeric), append(X, [C], Xs) }, number(Xs, NextX).
number(X, NextX) --> { length(X, L), L #> 0, number_chars(NextX, X) }.

report([X]) --> number(X), eol.
report([X|Xs]) --> number(X), " ", report(Xs).

input([]) --> call(eos).
input([L|Ls]) --> report(L), input(Ls).

% also from https://www.metalevel.at/prolog/dcg
include(Goal, List, Included) :-
        phrase(include_(List, Goal), Included).

include_([], _) --> [].
include_([L|Ls], Goal) -->
        (   { call(Goal, L) } ->
            [L]
        ;   []
        ),
        include_(Ls, Goal).
        
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

safe(Ls, S) :-
        findall(L,
                (   member(L, Ls),
                    (   mono_inc(L)
                    ;   mono_dec(L))),
                S).

% part 2 wip :(
safe_dampener(Ls, S) :-
                findall(L,
                (   member(L, Ls),
                    (   mono_inc(L)
                    ;   mono_dec(L)
                    ;   (   select(_, L, Ld),
                            mono_inc(Ld)
                        ;   mono_dec(Ld)))),
                S).

count_mono(Ls, Count) :-
        safe(Ls, Valid),
        length(Valid, Count).

count_mono_safe(Ls, Count) :-
        safe_dampener(Ls, Valid),
        length(Valid, Count).
        
/* -----------------------------------------------------------------------
   ?- phrase_from_file(input(Xs), "./day2.txt"), count_mono(Xs, Count).
   %@    Xs = [[38,41,44,47,50,47],[75,78,79,82,85,85],[11,13,16,19,21,25],[39,40,43,44,50],[75,77,80,78,80,83,84,87],[17,20,23,21,22,23,24,22],[80,82,79,80,82,82],[50,51,49,52,56],[78,80,82,83,80,81,82,88],[43,45,48,50,52,52,55],[34,35,38,38,41,39],[51,54,57,60,62,63,63,63],[24,26,27,30,33,33,34,38],[24,25,26,26,27,28,33],[33,35,37,41,44,46],[55,58,60,63,66|...],[90,93,94,98|...],[63,64,67|...],[13,15|...],[60|...]|...], Count = 371
   %@ ;  ... .
   ?- phrase_from_file(input(Xs), "./day2.txt"), count_mono_safe(Xs, Count).

----------------------------------------------------------------------- *
















