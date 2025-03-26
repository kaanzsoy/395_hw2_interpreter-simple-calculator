:- use_module(library(dcg/basics)).

% --- Grammar rules using DCG ---

% top level: addition, subtraction
expr(E) --> ws, term(T), expr_rest(T, E), ws.


expr_rest(Acc, E) --> ws, [43], ws, term(T), {Acc1 is Acc + T}, expr_rest(Acc1, E).
expr_rest(Acc, E) --> ws, [45], ws, term(T), {Acc1 is Acc - T}, expr_rest(Acc1, E).
expr_rest(E, E) --> [].

% multiplication, division
term(T) --> ws, expo(P), term_rest(P, T).


term_rest(Acc, T) --> ws, [42], ws, expo(P), {Acc1 is Acc * P}, term_rest(Acc1, T).
term_rest(Acc, T) --> ws, [47], ws, expo(P), {Acc1 is Acc / P}, term_rest(Acc1, T).
term_rest(E, E) --> [].


% Exponentiation (right-associative)
expo(E) --> primary(P), expo_rest(P, E).


expo_rest(Acc, E) --> ws, [94], ws, expo(P), {E is Acc ** P}.
expo_rest(Acc, Acc) --> [].


% numbers or parenthesized expressions - basic expr
primary(N) --> ws, number(N), ws.
primary(N) --> ws, [40], ws, expr(N), ws, [41], ws.


% ignore whitespace
ws --> [C], { code_type(C, space) }, ws.
ws --> [].


% --- REPL loop ---
repl :-
    write('Input:'), nl,
    read_line_to_codes(user_input, Codes),
    ( ( Codes = end_of_file ; Codes = [] )
      -> ( write('Exiting calculator!'), nl, halt )
      ;  ( phrase(expr(E), Codes)
           -> ( write('Output: '), write(E), nl )
           ;  ( write('Parse error'), nl )
          ),
          repl
      ).

:- initialization(repl).
