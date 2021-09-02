template_string_parse_transform
===

This parse transform adds template string support for erlang.

This code

```erlang
sayHello(Name) ->
    String = "Hello ${Name}!",
    io:format(String).
```

will be transformed to this: 

```erlang
sayHello(Name) ->
    String = io_lib:format("Hello ~p!", [Name]),
    io:format(String).
```
