-module(image_proc).

-export([start/0, init/2, process_image/0]).

-define(SERVER, ?MODULE).

-include("../include/offsetCalculation.hrl").

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

start() ->
    State = 0,
    Time = erlang:now(),
    Pid = spawn(?SERVER, init, [State, Time]),
    {ok, Pid}.

process_image() ->
    ?SERVER ! process_image,
    ok.

%%--------------------------------------------------------------------
%% Callback Definitions 
%%--------------------------------------------------------------------

%% @doc
%% Starts the module
init(State, Time) ->
    %% initialize 
    io:format("~p~n",[imgproc_nif:init()]),
    say("init", []),
    process(State, Time).

%%--------------------------------------------------------------------
%% Internal functions Definitions 
%%--------------------------------------------------------------------

process(State, Time) ->
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

	    case Processed of
		not_found ->
     		    not_found;
		_ ->
%%		    io:format("After processpic success ~n",[]),
%%		    DiffTime = diff_time(Time, erlang:now()),
%%		    io:format("IMAGE ~p : ~p ~n", [State, DiffTime]),    
		    %%ok
		    map_gen:add_frame(Processed, ?InputLaneD , 
				      {Car_Pos, Car_Tail, Car_Heading }, {State,Time})
	     end;
	_ ->
%%	    io:format("After getpic faile~n",[]),
	    not_found
    end,
    

    timer:sleep(17),

    process(State+1, erlang:now()).

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).


diff_time({_,_,MiS1},{_,_,MiS2}) -> 
    MiS2 - MiS1.
