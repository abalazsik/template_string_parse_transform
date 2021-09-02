-module(template_string_parse_transform).
-export([parse_transform/2]).

parse_transform(Forms, _) ->
	Intermediate = postorder(
			erl_syntax:form_list(Forms)),
	erl_syntax:revert_forms(Intermediate).

postorder(Tree) ->
    processForm(case erl_syntax:subtrees(Tree) of
        [] -> Tree;
        List -> erl_syntax:update_tree(Tree,
                [[postorder(Subtree)
                    || Subtree <- Group]
                    || Group <- List])
        end).

processForm(Record) ->
    case erl_syntax:type(Record) of
        string ->
            String = erl_syntax:string_value(Record),
            case processString(String) of
                {FormattedString, Parameters} ->
                    makeFormatFunctionCall(FormattedString, Parameters);
                _ -> Record
            end;
        _ -> Record
    end.

makeFormatFunctionCall(FormattedString, Parameters) ->
    erl_syntax:application(erl_syntax:atom(io_lib), erl_syntax:atom(format),
        [
            erl_syntax:revert(erl_syntax:string(FormattedString)),
            toList(Parameters)
        ]
    ).

toList([]) -> erl_syntax:revert(erl_syntax:nil());
toList([Atom | Rest]) -> 
	erl_syntax:revert(
		erl_syntax:cons(
			erl_syntax:variable(Atom),
			toList(Rest)
			)
	).

processString(String) when is_list(String) ->
    case parse(String) of
        [String] ->
            String;
        Terms ->
            Parameters = lists:filtermap(
                fun
                    ({param, Name}) -> {true, Name};
                    ({param, Name, _}) -> {true, Name};
                    (_) -> false
                end,
                Terms
            ),
            Formatted = lists:flatten(
                    lists:map(
                        fun 
                            ({param, _}) -> "~p";
                            ({param, _, Format}) -> format(Format);
                            (List) when is_list(List) -> List
                        end,
                        Terms
                    )
            ),
            {Formatted, Parameters}
    end.

format("str") -> "~s";
format("s") -> "~s";
format("e") -> "~e";
format("f") -> "~f";
format("g") -> "~g";
format("w") -> "~w";
format("p") -> "~p";
format(Format) -> erlang:throw({unknown_format, Format}).

%=====PARSER(here comes the fun part)=====

parse(String) ->
    try
        parse([], [], String)
    catch error:_ ->
        erlang:throw({invalid_string, String})
    end.

parse(Words, Current, [$$, ${, Ch | Rest]) when (Ch >= $A) andalso (Ch =< $Z) ->
	parse([lists:reverse(Current)] ++ Words, {param, [Ch]}, Rest);
parse(Words, {param, Current}, [$} | Rest]) ->
	parse([{param, lists:reverse(Current)}] ++ Words, [], Rest);
parse(Words, {param, Current, ParamType}, [$} | Rest]) ->
	parse([{param, lists:reverse(Current), lists:reverse(ParamType)}] ++ Words, [], Rest);
parse(Words, {param, Current, ParamType}, [Ch | Rest]) when (Ch >= $a) andalso (Ch =< $z) ->
	parse(Words, {param, Current, [Ch] ++ ParamType}, Rest);
parse(Words, {param, Current}, [$:, Ch | Rest]) when (Ch >= $a) andalso (Ch =< $z) ->
	parse(Words, {param, Current, [Ch]}, Rest);
parse(Words, {param, Current}, [Ch | Rest]) when ((Ch >= $a) andalso (Ch =< $z)) orelse ((Ch >= $A) andalso (Ch =< $Z)) orelse ((Ch >= $0) andalso (Ch =< $9)) orelse Ch == $_ ->
	parse(Words, {param, [Ch] ++ Current}, Rest);
parse(Words, Current, [Ch | Rest]) ->
	parse(Words, [Ch] ++ Current, Rest);
parse(Words, Current, []) ->
	lists:reverse([lists:reverse(Current)] ++ Words).


%TESTS

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

processString_test_() ->
    [
        ?_assertEqual({"Hello ~p", ["World"]}, processString("Hello ${World}")),
        ?_assertEqual({"param1 = ~p, param2 = ~s", ["Param1", "Param2"]}, processString("param1 = ${Param1}, param2 = ${Param2:s}"))
    ].

negative_processString_test_() -> 
    [
        ?_assertThrow({invalid_string, "Hello ${Wo${r}ld}"}, processString("Hello ${Wo${r}ld}")),
        ?_assertThrow({invalid_string, "Hello ${World"}, processString("Hello ${World")),
        ?_assertThrow({unknown_format, "x"}, processString("Hello ${World:x}"))
    ].

parser_test_() ->
    [
        ?_assertEqual(
            ["this ", {param,"Is","ws"}, " a text. This is a ", {param,"Second"}, " param"],
            parse("this ${Is:ws} a text. This is a ${Second} param")
            ),
        ?_assertEqual(["This is a normal string"],
            parse("This is a normal string")
            )
    ].

-endif.