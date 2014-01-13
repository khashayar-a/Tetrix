-module(image_proc).

-export([start/0, init/1, process_image/0]).

-define(SERVER, ?MODULE).

-include("../include/offsetCalculation.hrl").

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start() ->
    State = 0,
    Pid = spawn(?SERVER, init, [State]),
    {ok, Pid}.

process_image() ->
    ?SERVER ! process_image,
    ok.

%%--------------------------------------------------------------------
%% Callback Definitions 
%%--------------------------------------------------------------------

%% @doc
%% Starts the module
init(State) ->
    %% initialize 
    io:format("~p~n",[imgproc_nif:init()]),
    say("init", []),
    process(State).

%%--------------------------------------------------------------------
%% Internal functions Definitions 
%%--------------------------------------------------------------------

process(State) ->
    %% get car position vehicle_data
    Car_Pos = vehicle_data:car_position(),
    Car_Heading = vehicle_data:car_heading(),
    Car_Tail = vehicle_data:car_tail(),
    %% %% Side = map_gen:road_side(),
    %%    io:format("After vehicle data~n",[]),
    %% %% query frame
    case imgproc_nif:get_pic() of
     	{ok, ImgRef} ->
%%	    io:format("After getpic success~n",[]),
	    Processed = imgproc_nif:process_pic(ImgRef, State),
%%	    io:format("After processpic success ~n",[]),
	    case Processed of
		not_found ->
     		    not_found;
		_ ->
		    %%ok
		    map_gen:add_frame(Processed, ?InputLaneD , {Car_Pos, Car_Tail, Car_Heading })
	     end;
	_ ->
%%	    io:format("After getpic faile~n",[]),
	    not_found
    end,
    
    timer:sleep(30),
    process(State+1).

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
