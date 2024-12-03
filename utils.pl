:- module(utils, [eol//0,
                  eos/2,
                  number//1,
                  include/3]).

:- use_module(library(between)).
:- use_module(library(builtins)).
:- use_module(library(charsio)).
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
        