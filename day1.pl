% aoc 24 day1 - scryer-prolog

:- use_module(library(pairs)).
:- use_module(library(builtins)).
:- use_module(library(csv)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

% from https://github.com/mthom/scryer-prolog/blob/master/src/lib/ugraphs.pl#L134
msort_(List, Sorted) :-
        pairs_keys(Pairs, List),
        keysort(Pairs, SortedPairs),
        pairs_keys(SortedPairs, Sorted).

% not sure how to parse the csv the way I want, so grab the first & last elem
list_first_last(List, [First, Last]) :-
        append([First | _Middle], [Last], List).

% pairwise absolute diff b/w two lists
abs_delta([], _, []).
abs_delta(_, [], []).
abs_delta([Hx|Tx], [Hy|Ty], [Sub|R]) :-
        Sub is abs(Hx - Hy),
        abs_delta(Tx, Ty, R).

/* -----------------------------------------------------------------------
   ?- abs_delta([1,2,3], [4,5,6,7], R).
   %@    R = [3,3,3]
   %@ ;  ... .
----------------------------------------------------------------------- */

% is there a more 'prolog' way to do this?
count_occurrences(List, Elem, Count) :-
        findall(X, (member(X, List), X =:= Elem), Occurs),
        length(Occurs, Count).

% this seems super dirty as well, is a hashset of sorts, better?
occurrences(Set, List, Res) :-
        findall(count(Elem, Count), (member(Elem, Set), count_occurrences(List, Elem, Count)), Res).

/* -----------------------------------------------------------------------
   ?- count_occurrences([], 4, Count).
   %@    Count = 0.
   
   ?- List = [1,2,2,3,3,3,4,3,5], findall(X, (member(X, List), X =:= 3), Occs), length(Occs, Res).
   %@    List = [1,2,2,3,3,3,4,3,5], Occs = [3,3,3,3], Res = 4.
   
   ?- occurrences([2,3], [1,2,2,3,2,3,2,3,4], Res), multiply_and_sum(Res, Total).
   %@    Res = [count(2,4),count(3,3)], Total = 17.
----------------------------------------------------------------------- */

% computation for part 2
multiply_and_sum(Res, Total) :-
        findall(Product, (member(count(Elem, Count), Res), Product is Elem * Count), Products),
        sum_list(Products, Total).

% read the csv, sep by whitespace, but idk how to do that properly,
% so manually grab first and last elem of each "phrase" which
% represent the rows
read_part1(File, Cols) :-
        phrase_from_file(parse_csv(frame(_, Rows), [with_header(false), token_separator(' ')]), File),
        maplist(list_first_last, Rows, Acc),
        transpose(Acc, Cols).

day1_part1(File, Res) :-
        read_part1(File, Cols),
        maplist(msort_, Cols, [L1, L2]),
        abs_delta(L1, L2, SubRes),
        sum_list(SubRes, Res).
        
day1_part2(File, Res) :-
        read_part1(File, [L1, L2]),
        list_to_set(L1, SetL1),
        occurrences(SetL1, L2, Counts),
        multiply_and_sum(Counts, Res).

/* -----------------------------------------------------------------------
   ?- read_part1('./day1.csv', [Col1, Col2]).
   %@    Col1 = [15131,32438,12503,73808,57168,97870,18072,55097,36615,63626,19535,20386,32817,90111,81180,20278,71822,36650,96658,48953|...], Col2 = [78158,35057,57702,43128,71761,29344,79079,92997,67927,85851,91599,53482,55364,84813,13958,82963,70030,74064,93643,12223|...]
   %@ ;  ... .

   ?- day1_part1('./day1.csv', Res).
   %@    Res = 2367773
   %@ ;  ... .
   
   ?- day1_part2('./day1.csv', Res).
   %@    Res = 21271939
   %@ ;  ... .
   
   ?- list_to_set([1,2,2,3], S).
   %@    S = [1,2,3].

   ?- list_to_set([1,2,2,3], S), occurrences(S, [1, 2, 2, 2, 1, 3, 1, 2, 3], Counts), multiply_and_sum(Counts, R).
   %@    S = [1,2,3], Counts = [count(1,3),count(2,4),count(3,2)], R = 17.
      
% from https://cs.union.edu/~striegnk/learn-prolog-now/html/node51.html#subsec.l6.reverse.acc % %
   acc_rev([], Acc, Acc).
   acc_rev([H|T], Acc, Res) :- acc_rev(T, [H|Acc], Res).

   ?- acc_rev([1, 2, 3, 9], [], Res).
----------------------------------------------------------------------- */