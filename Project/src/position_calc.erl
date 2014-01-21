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
    {ok, P} = python:start([{python_path, "/home/tetrix/Tetrix/Project/py_source"},
			    {python, "python"}]),
    python:call(P, hkodgpio, start_it, []),
    %% initialize 
    say("init", []),
    locate(P).

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

locate(P) ->
    case python:call(P, hkodgpio, get_movement, []) of
	0 ->
	    ok;
	Movement ->
	    io:format("HAL READ : ~p~n",[Movement]),
	    gen_server:cast(vehicle_data, {hal_moved, Movement})
    end,
    timer:sleep(1),
    locate(P).

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
