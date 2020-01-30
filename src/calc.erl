-module(calc).
-export([rpn/1]).

rpn(String)->rpncalc_implementation(string:tokens(String, " ")).

rpncalc_implementation(Tokens) ->
    try lists:foldl(fun operate/2, [], Tokens) of
        [A] -> A;
        _ -> invalid_expression
    catch
        _:_ -> invalid_expression
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
operate(Number, Stack) ->
    try erlang:list_to_float(Number) of
        Var -> [Var | Stack]
    catch
        _:_ -> [erlang:list_to_integer(Number) | Stack]
    end.