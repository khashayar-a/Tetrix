-module(position_calc).

%% API
-export([start/0, init/0, calculate_car_position/0]).

-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start() ->
    Pid = spawn(?SERVER, init, []),
    register(?SERVER, Pid),
    {ok, Pid}.

% @doc
% Calculates car position, and sends it to vehicle_data server
calculate_car_position() ->
    ?SERVER ! calculate_car_position,
    ok.

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init() ->
    %% {ok,gpio_nif:start()},
    %% gpio_nif:init_gpio_pins(),
    %% say("init", []),
    %% locate().

    {ok, P} = python:start([{python_path, "/home/tetrix/Tetrix/Project/py_source"},
			    {python, "python"}]),
    python:call(P, hkodgpio, start_it, []),
%% initialize 
    say("init", []),
    locate(P, erlang:now(), 0).


%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

locate(P,Time, Total) ->
     %% case gpio_nif:get_movement() of
     %%     0 ->
     %%         ok;
     %%     Movement ->
     %% 	   io:format("HAL READ : ~p~n",[Movement]),
     %%         gen_server:cast(vehicle_data, {hal_moved, Movement})
     %% end,
     %% timer:sleep(1),
     %% locate().
    case python:call(P, hkodgpio, get_movement, []) of
	0 ->
	    NewTime = Time,
	    NewTotal = Total,
	    ok;
	Movement ->
	    io:format("HAL READ : ~p ~n",[Movement]),
	    gen_server:cast(vehicle_data, {hal_moved, Movement}),
	    case Movement + Total > 250 of
		true ->
		    Speed = (Movement + Total) * 0.00525 / (diff_time(Time, erlang:now()) / 1000000),
		    io:format(" OMG SPEED IS : ~p ~n", [Speed]),
		    NewTime = erlang:now(),
		    NewTotal = 0;
		false ->
		    NewTotal = Total + Movement,
		    NewTime = Time
	    end
    end,
    timer:sleep(1),
    locate(P, NewTime, NewTotal).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

diff_time({_,S1,MiS1},{_,S2,MiS2}) -> 
    ((S2-S1) * 1000000) + (MiS2 - MiS1).
