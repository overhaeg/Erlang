-module(hello_world).
-export([start/0, print/2]).

start() ->
    HelloWorld = "Hello World!",
    [First | Rest ] = lists:map(fun printer/1, HelloWorld),
	First ! {go, Rest}, 
    timer:sleep(1000),
    io:nl().

printer(Char) ->
    spawn(?MODULE, print, [Char, random:uniform(500)]).

print(Char, Sleep) ->
	timer:sleep(Sleep),
	receive
		{go, [F | Rest]} -> 
			io:put_chars([Char]),
			F ! {go, Rest};
		{go, []} -> io:put_chars([Char])
	end.
