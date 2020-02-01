-module(calc).
-export([evaluate/1]).

evaluate(String) ->
    case evaluate(postfix, String) of
        invalid_expression -> 
            try evaluate(infix, String) of
                Var -> Var
            catch
                _:_ -> invalid_notation
            end;
        Var -> Var
    end.

evaluate(postfix, String)->rpncalc_implementation(string:tokens(String, " "));
evaluate(infix, String)->rpncalc_implementation(parser(tokenizer(String), []));
evaluate(_, _) -> invalid_notation.

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
operate(StringRepresentation, Stack) when is_list(StringRepresentation) -> [get_number(StringRepresentation) | Stack];
operate(Number, Stack) when is_integer(Number) -> [Number | Stack].


%% Transform to RPN

tokenizer(Str) ->    
     {ok, Tokens, _} = erl_scan:string(Str ++ "."),
     {ok, [E]} = erl_parse:parse_exprs(Tokens),
     E.

parser({op, _, What, LS, RS}, RPN) ->
    New_RPN = parser(LS, RPN),
    Result = parser(RS, New_RPN),
    Result ++ [atom_to_list(What)];
parser({integer, _, N}, RPN) -> RPN ++ [N];
parser({float, _, N}, RPN) -> RPN ++ [N].


%% Helper functions
get_number(StringRepresentation) ->
    try erlang:list_to_float(StringRepresentation) of
        Var -> Var
    catch
        _:_ -> erlang:list_to_integer(StringRepresentation)
    end.