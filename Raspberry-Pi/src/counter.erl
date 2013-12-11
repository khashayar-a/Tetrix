-module(counter).
-export([start/0,loop/3]).

start() ->
    gpio_sup:start_link([{23, input}]),
    Pid=self(),
    register(?MODULE, spawn(counter,loop,[0,0,Pid])).

loop (Count,State,Pid) ->
    A=gpio:read(23),
    case A == State of 
	true -> Tmp = Count; 
	false -> 
	    Tmp = Count + 1, 
	    io:format("COunt : ~p~n" , [ Tmp])
    end,
    %%Pid ! {Tmp},
    loop(Tmp,A,Pid).

