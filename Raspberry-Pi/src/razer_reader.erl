-module(razer_reader).

-export([start/0, init/0]).

start() ->
    spawn(razer_reader, init, []).

init() ->
    {ok, _Pid}  = i2c_sup:start_link([{i2c0, "/dev/i2c-0"}]),
    loop(0).

loop(Prev_Heading) ->
    timer:sleep(20),
    case get_data() of
	bad_data ->
	    loop(Prev_Heading);
	Prev_Heading ->
	    loop(Prev_Heading);
	New_Heading ->
	    gen_server:cast(hardware, {razer, New_Heading}),
	    loop(New_Heading)
    end.
		
	    


get_data() ->
    X = i2c:read(i2c0, 100, 2),
    case list_to_binary(tuple_to_list(X)) of
	<<84:7, A:9>> ->
	    A;
	_ ->
	    bad_data
    end.



%% %%before 
%%     case razer_nif:get_byte() of
%% 	-1 ->
%% 	    loop(Prev_Heading , Buff);
%% 	$% ->
%% 	    loop(Prev_Heading , Buff);
%% 	$| ->
%% 	    case list_to_integer(Buff) == Prev_Heading of
%% 		true ->
%% 		    loop(list_to_integer(Buff), []);
%% 		false ->  
%% %%		    io:format("Razer Update: ~p ~n" , [list_to_integer(Buff)]),
%% 	    	    gen_server:cast(hardware, {razer, list_to_integer(Buff)}),
%% 		    loop(list_to_integer(Buff), [])
%% 	    end;
%% 	H ->
%% 	    loop(Prev_Heading , Buff ++ [H])
%%     end.

