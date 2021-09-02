-module(hello).
-export([sayHello/1, sayTypedHello/1]).

-compile({parse_transform, template_string_parse_transform}).

sayHello(Name) ->
    String = "Hello ${Name}!~n",
    io:format(String).

sayTypedHello(Name) ->
    String = "Hello ${Name:s}!~n",
    io:format(String).
