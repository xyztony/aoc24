:- use_module('../../utils').

:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).


mul_expr(Product) --> "mul(", number(X), ",", number(Y), ")", { Product #= X * Y }.

sum_products(Ps, T) --> sum_products(Ps), { sum_list(Ps, T) }.
sum_products([Ps0|Ps]) --> ..., mul_expr(Ps0), sum_products(Ps).
sum_products([]) --> ..., eol.

part1(File, TotalSum) :-
    phrase_from_file(sum_products(_, TotalSum), File).

/* -----------------------------------------------------------------------
   ?-  phrase(sum_products(_, T), "helloworldmul(1,2) kjdsfkfsamul(4,2)83290*saj309#@)mul(20,42)").
   %@    T = 850
   %@ ;  ... .
   
   ?- part1("./day3.txt", Total).
   %@    Total = 187833789
   %@ ;  ... .
      
----------------------------------------------------------------------- */


sum_products_c(_, Ps, T)       --> sum_products_c(true, Ps), { sum_list(Ps, T) }.
sum_products_c(_, Ps)          --> "don't()", sum_products_c(false, Ps). % safety - next mul expr doesnt count
sum_products_c(true, [Ps0|Ps]) --> mul_expr(Ps0), sum_products_c(true, Ps). % next mul expr counts
sum_products_c(_, Ps)          --> "do()", sum_products_c(true, Ps). % do found, go fetch next mul
sum_products_c(State, Ps)      --> [_], sum_products_c(State, Ps). % nothing found? keep going with current state
sum_products_c(_, [])          --> ..., eol.

% "At the beginning of the program, mul instructions are enabled."
part2(File, TotalSum) :-
        phrase_from_file(sum_products_c(true, Ps, TotalSum), File).


/* -----------------------------------------------------------------------
   
   ?-  phrase(sum_products_c(true, Ps, T), "mul(1,2)don't()mul(3,4)mul(6,9)do()mul(2,3)").
   %@    Ps = [2,6], T = 8
   %@ ;  ... .

   ?- part2("./day3part2.txt", T).
   %@    T = 94455185
   %@ ;  ... .

----------------------------------------------------------------------- */