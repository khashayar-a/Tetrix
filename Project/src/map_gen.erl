-module(map_gen).

-behaviour(gen_server).

%% Internal functions
-export([say/2]).
-compile(export_all).

%% API
-export([start_link/0, node_ahead/1, add_frame/3, road_side/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {node_ahead, road_side, frame_data, matrix_id, camera_matrix, mode, last_dashes , debug}).
-record(dash_line, {center_point, box, points, area, dash_before, dash_after}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("include/undistort.txt"),
    %%Camera_Matrix = read_cm_file("include/camera_matrix.txt"),

    ets:new(dash_lines, [set, named_table]),

    % Dummy values for the state 
    {ok, #state{road_side = right, node_ahead = {{0,0},{0,0},{0,0}},
		matrix_id = ID , mode = start, debug = on}}.

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Argument is a tuple containing X and Y coords, i.e. Car_Pos = {5,6}
node_ahead(Car_Pos) ->
    gen_server:call(
      ?SERVER,
      {node_ahead, Car_Pos}).


% @doc
% Adds frame data from image processing. Arguments are Points detected, and Car
% position 
add_frame(Points, Lane_ID, Car_Pos) ->
    gen_server:cast(
      ?SERVER,
      {add_frame, {{Points, Lane_ID}, Car_Pos}}).

debug(N) ->
    gen_server:cast(?SERVER, {debug, N}).

% @doc
% Retrieves the side of the road that the vehicle is on
road_side() ->
    gen_server:call(?SERVER, road_side).

save_tab() ->
    ets:tab2list(dash_lines).

write_tab(N) ->
    {ok, Log}  = file:open("savedtab" , [read, write]),
    T = map_functions: separate(ets:tab2list(dash_lines), [], N),
    io:fwrite(Log, "~p~n" , [T]),
    file:close(Log).

last_dashes() ->
        gen_server:call(?SERVER, last_dashes).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call({node_ahead,{CarX,CarY}}, _From, State) ->
    % TODO: generate node ahead with {X,Y} values, using dummy values
    %% query ets for node ahead
    Reply = State#state.node_ahead,
    {reply, Reply, State};

handle_call(road_side, _From, State) ->
    Reply = State#state.road_side,
    {reply, Reply, State};

handle_call(last_dashes, _From, State) ->
    Reply = State#state.last_dashes,
    {reply, Reply, State};

handle_call(time_to_terminate, _From, State) ->
    ets:tab2file(dash_lines, "dashline"),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos, Car_Tail, Car_Heading}}}, 
	    State#state{mode = recording}) ->
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    
    Correction = calculate_correct_pos([Dash| T] , State#state.last_dashes),
    case State#state.debug of
	on ->
	    io:format("___________________________________________________________~n", []),
	    io:format("Car Stuff ~p , ~p ~n" , [Car_Pos, Car_Heading]),
	    io:format("NEW DASHES : ~p~nLast~p~n" , [Temp_Dashes, State#state.last_dashes]),
	    io:format("Correction : ~p~n", [Correction]);	
	_ ->
	    ok
    end,
    
    case Correction of 
	not_found ->
	    ok;
	{Dashes_Needed, Corresponding_Dash, {Offset, Delta_Angle}} ->
	    Movement = map_functions:getDistance({0,0}, Offset),
	    case Movement > 5 of
		false ->
		    ok;
		true ->
		    Moved_Dashes = map_functions:move_dashes(Dashes_Needed, 
							     {Corresponding_Dash#dash_line.center_point,
							      {Offset,Delta_Angle}}, []),
		    case State#state.debug of
			on ->
			    io:format("Moved_Dashes : ~p~n", [Moved_Dashes]);	
			_ ->
			    ok
		    end
	    end
    end,
    {noreply, State#state{mode = recording, last_dashes = map_functions:get_last_dashes(),
			  debug = off}};



handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos, Car_Tail, Car_Heading}}}, 
	    State#state{mode = start}) ->
    
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    NewDashes = map_functions:connect_dashes(Temp_Dashes, undef, []), 
    io:format("START MODE ~p , ~p ~n" , [Car_Pos, Car_Heading]),
    io:format("Temp DASHES : ~p~n" , [Dashes]),
    io:format("NEW DASHES : ~p~n" , [NewDashes]),
    map_functions:insert_dashes(NewDashes),
    {noreply, State#state{mode = recording, last_dashes = map_functions:get_last_dashes(),
			  debug = on}};



    %% query ets to get closest dash to each

    %% calculate exact car pos

    %% update dashes
    
    %% calculate offset nodes


%%     New_Dashes = translate_dashes(State#state.matrix_id, Dashes, {{4,-67},math:pi()/2},[]),
    
%%     case New_Dashes of 
%%         [] ->
%%             {noreply, State};
%%         [_] ->
%%             {noreply, State};
%%         [H,T] ->
%%             P1 = hd(H#dash_line.points),
%%             P3 = lists:Nth(3, T#dash_line.points),
%%             P2 = center_point(lists:nth(3, H#dash_line.points), hd(T#dash_line.points)),
%%             {ok,[OP1]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, H#dash_line.points),
%%             {ok,[OP2]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, [P1,P2,P3]),
%%             {ok,[OP3]} = offsetCalculation:calculate_offset_list(Line_ID, ?LaneAdjacent, T#dash_line.points),
             
%%             Node_List = [OP1, OP2, OP3],
%%             car_ai:start(Node_List),                         
%%             {noreply, State#state{node_ahead = Node_List, frame_data = Node_List ,mode = recording}};
%%         _ ->
%%             Node_List = calculate_offsets(Line_ID, ?LaneAdjacent, NewDashes, []),
%%             car_ai:start(Node_List),
%%             {noreply, State#state{node_ahead = Node_List, frame_data = Node_List , mode = recording}}
%%     end;

handle_cast({debug, N}, State) ->
    {noreply, State#state{debug = N} };
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    % Unexpected messages, i.e. out-of-band
    error_logger:info_msg("Unexpected message:~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating:~p~n", [?MODULE]),

    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [ _OldVsn, State, _Extra]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

