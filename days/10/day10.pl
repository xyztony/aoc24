:- use_module('../../utils').
:- use_module(library(between)).
:- use_module(library(builtins)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(pio)).
:- use_module(library(tabling)).
:- use_module(library(time)).

:- dynamic(dayten/3).
at(X-Y, V) :- asserta(dayten(X-Y, V)).

grid([]) --> call(eos).
grid([H|T]) --> row(H), grid(T).

row([]) --> ( "\n" | call(eos) ), !.
row([H|T]) --> height(H), row(T).
height(H) --> [C], { char_type(C, numeric), number_chars(H, [C]) }.

load_grid(Rows) :- load_grid(Rows, 0).
load_grid([], _).
load_grid([Row | Rows], Y) :-
        load_grid_(Row, Y, 0), Y1 #= Y + 1, load_grid(Rows, Y1).
load_grid_([], _, _).
load_grid_([H | Hs], Y, X) :-
        at(X-Y, H), X1 #= X + 1, load_grid_(Hs, Y, X1).

possible_path(X-Y, X1-Y) :- X1 #= X - 1 ; X1 #= X + 1.
possible_path(X-Y, X-Y1) :- Y1 #= Y - 1 ; Y1 #= Y + 1.

next_path(X-Y, Xn-Yn) :-
        dayten(X-Y, N),
        Nn #= N + 1,
        possible_path(X-Y, Xn-Yn),
        dayten(Xn-Yn, Nn).

path(From, To) :-
    dayten(From, N),
    (   between(0,7,N)
    ->  possible_path(From, Next),
        dayten(Next, Nn),
        Nn #= N + 1,
        path(Next, To)
    ;   N #= 8,
        possible_path(From, To),
        dayten(To, 9)
    ).

main(Part1, Part2) :-
        retractall(dayten(_,_)),
        phrase_from_file(grid(M), "./part1.txt"),
        load_grid(M),
        countall((dayten(Pos, 0), path(Pos, To)), Part1),
        countall((setof(Score, (dayten(Pos, 0), path(Pos, To)), Scores)), Part2).

?- time(main(P,Z)).
