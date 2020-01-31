-module(calc).
-export([rpn/1, calc/1]).

rpn(String)->rpncalc_implementation(string:tokens(String, " ")).
% calc(String)->rpncalc_implementation(parse_to_polish(string:tokens(String, " "))).
calc(String)->parse_to_polish(string:tokens(String, " ")).


rpncalc_implementation(Tokens) ->
    try lists:foldl(fun operate/2, [], Tokens) of
        [A] -> A;
        _ -> invalid_expression
    catch
        _ -> invalid_expression
    end.

operate("+", [A, B | T]) -> [B + A | T];
operate("*", [A, B | T]) -> [B * A | T];
operate("-", [A, B | T]) -> [B - A | T];
operate("/", [A, _ | _]) when A == 0 -> throw(div_by_zero);
operate("/", [A, B | T]) -> [B / A | T];
operate("^", [A, B | T]) -> [math:pow(B, A) | T];
operate("ln", [A | T]) -> [math:log(A) | T];
operate("log10", [A | T]) -> [math:log10(A) | T];
operate("sum", Stack) -> [lists:sum(Stack)];
operate("prod", Stack) -> [lists:foldl(fun erlang:'*'/2, 1, Stack)];
operate(StringRepresentation, Stack) when is_list(StringRepresentation) -> [get_number(StringRepresentation) | Stack];
operate(Number, Stack) when is_integer(Number) -> [Number | Stack].


%% Transform to RPN

plus_minus([]) -> [];
plus_minus(["+" | Tokens]) -> plus_minus(Tokens) ++ ["+"];
plus_minus(["-" | Tokens]) -> plus_minus(Tokens) ++ ["-"];
plus_minus(Tokens) -> mul_div(Tokens).

mul_div([]) -> [];
mul_div(["*" | Tokens]) -> plus_minus(Tokens) ++ ["*"];
mul_div(["/" | Tokens]) -> plus_minus(Tokens) ++ ["/"];
mul_div(Tokens) -> symbols(Tokens).

symbols([]) -> [];
symbols(["(" | Tokens]) -> plus_minus(Tokens);
symbols([")" | Tokens]) -> plus_minus(Tokens);
symbols([StringRepresentation | Tokens]) -> [get_number(StringRepresentation) | plus_minus(Tokens)].

parse_to_polish(Tokens) -> plus_minus(Tokens).


%% Helper functions

get_number(StringRepresentation) ->
    try erlang:list_to_float(StringRepresentation) of
        Var -> Var
    catch
        _:_ -> erlang:list_to_integer(StringRepresentation)
    end.