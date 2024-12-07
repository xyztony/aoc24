:- module(utils, [number//1,
                  eol//0,
                  concat_nums/3,
                  eos/2,                   
                  include/3]).

:- use_module(library(between)).
:- use_module(library(builtins)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

number(X) --> number([], X).
number(X, NextX) --> [C], { char_type(C, numeric), append(X, [C], Xs) }, number(Xs, NextX).
number(X, NextX) --> { length(X, Xs), Xs #> 0, number_chars(NextX, X) }.

% from https://www.metalevel.at/prolog/dcg
eol --> ("\n"; call(eos)).
eos([], []).

lines([])     --> call(eos), !.
lines([L|Ls]) --> line(L), lines(Ls).

line([])     --> ( "\n" | call(eos) ), !.
line([C|Cs]) --> [C], line(Cs).

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

transform([Xs, Ys], Zs) :- transform(Xs, Ys , Zs).
transform([], [], []).
transform([X|Xs], [Y|Ys], [X,Y|Zs]) :- transform(Xs,Ys,Zs).

splitSet([],[],[]).
splitSet([_|T],[_|L],R) :-
        splitSet(T,L,R).
splitSet([_|T],L,[_|R]) :-
        splitSet(T,L,R).


concat_nums(X, Y, R) :-
        num_digits(Y, DY), R #= (X * 10 ^ DY) + Y.

num_digits(N, D) :-
        num_digits_(N, 0, D).
num_digits_(N, D0, D) :-
        N #=< 9, N #>= 0, D is D0 + 1.
num_digits_(N, D0, D) :-
        N #> 9,
        D1 #= D0 + 1,
        N1 #= N // 10,
        num_digits_(N1, D1, D).