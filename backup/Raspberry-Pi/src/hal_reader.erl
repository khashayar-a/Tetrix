-module(hal_reader).

-export([start/0, init/0]).


start() ->
    gpio_sup:start_link([{23, input}]),   
    spawn(hal_reader, init, []).

init() ->
    loop(0,0).

loop(Count, State) ->
    timer:sleep(10),
    A = gpio:read(23),
    case A == State of 
	true -> 
	    loop(Count,State);
	false -> 
	    gen_server:cast(hardware, {hal, Count +1}),
	    loop(Count+1, A)
    end.
