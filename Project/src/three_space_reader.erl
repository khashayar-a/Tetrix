-module(three_space_reader).

-export([start/0, init/0]).

start() ->
    Pid = spawn(three_space_reader, init, []),
    {ok, Pid}.

init() ->
    %% case three_space_nif:load() of
    %% 	1 ->
    %% 	    ok;
    %% 	0 ->
    %% 	    not_ok
    %% end,
    loop(0).

loop(Prev_Heading) ->
    timer:sleep(20),
    case three_space_nif:get_heading() of
	bad_data ->
	    loop(Prev_Heading);
	Prev_Heading ->
	    loop(Prev_Heading);
	New_Heading ->
%%	    io:format("RAZOR READ : ~p~n", [New_Heading]),
	    gen_server:cast(vehicle_data, {angle, New_Heading}),
	    loop(New_Heading)
    end.

