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
		matrix_id = ID , mode = start, debug = off}}.

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

handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos, Car_Tail, Car_Heading}}}, State) ->

    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    NewDashes = map_functions:connect_dashes(Temp_Dashes, undef, []), 
    

    case State#state.mode of
	start ->
	    io:format("START MODE ~p , ~p ~n" , [Car_Pos, Car_Heading]),
	    io:format("Temp DASHES : ~p~n" , [Dashes]),
	    io:format("NEW DASHES : ~p~n" , [NewDashes]),
	    map_functions:insert_dashes(NewDashes),
	    {noreply, State#state{mode = recording, last_dashes = map_functions:get_last_dashes() , debug = off}};
 	recording ->
 	    Correction = map_functions:calculate_correct_pos(NewDashes , State#state.last_dashes),  
 	    case Correction of
		not_found ->
%%		    io:format("N" , []),
		    case State#state.debug of
			on ->
			    io:format("NEW DASHES : ~p~nLast~p~n" , [NewDashes, State#state.last_dashes]);
			_ ->
			    ok
		    end,
		    Estimated_Dashes =
			map_functions:generate_estimated_dashes(State#state.last_dashes),  
		    Estimated_Correction = 
			map_functions:calculate_estimated_correction(NewDashes , Estimated_Dashes),
		    Last_Dash = map_functions:get_last(State#state.last_dashes),
		    Distance = map_functions:getDistance(Last_Dash#dash_line.center_point,
							 (hd(NewDashes))#dash_line.center_point),
		    case {Distance > 500 , Estimated_Correction} of
			{_, not_found} ->
			    {noreply, State#state{mode = recording, debug = off}};
			{false, _} ->
			    {noreply, State#state{mode = recording, debug = off}};
			{true, {Center_Point = {Cx,Cy}, {Offset={Ox,Oy}, Delta_Angle}}} ->
			    io:format("EsTimation : ~p~n", [Estimated_Correction]),
			    Moved_Dashes = map_functions:move_dashes(NewDashes, 
								     Estimated_Correction, []),
			    Head_Dash = hd(Moved_Dashes),
			    ets:insert(dash_lines, 
				       {Last_Dash#dash_line.center_point, 
					Last_Dash#dash_line{
					  dash_after = Head_Dash#dash_line.center_point}
				       }),
			    Connected_Dashes = 
				map_functions:connect_dashes(Moved_Dashes,
							     Last_Dash#dash_line.center_point, []), 
			    map_functions:insert_dashes(Connected_Dashes),
			    New_CarPos = map_functions:move_point(Car_Pos, Estimated_Correction),
			    New_CarHeading = Car_Heading  + Delta_Angle,
			    gen_server:cast(vehicle_data, 
					    {correct_position, New_CarPos , New_CarHeading}),
			    {noreply, State#state{mode = recording , 
						  last_dashes = map_functions:get_last_dashes(), 
						  debug = off}}
		    end;
		{Center_Point = {Cx,Cy}, {Offset={Ox,Oy}, Delta_Angle}} ->
		    %% io:format("Correction found ~p~n", [Correction]),
		    Dashes_Needed = map_functions:remove_dashes_before({Cx-Ox, Cy-Oy}, NewDashes),
  		    Moved_Dashes = map_functions:move_dashes(Dashes_Needed, Correction, []),
  		    Orig_Dash = map_functions:ets_lookup(Center_Point),
		    %% case Orig_Dash#dash_line.dash_before of
		    %% 	undef ->
		    %% 	    ok;
		    %% 	Before_Point ->
		    %% 	    Before_Dash = map_functions:ets_lookup(Before_Point),

		    %% end,
		    
		    Connected_Dashes = map_functions:connect_dashes(tl(Moved_Dashes),
								    Orig_Dash#dash_line.center_point,
								    []),
		    case Connected_Dashes of 
			[] ->
			    ow_well;
			[Head_Dash| _] ->
			    ets:insert(dash_lines, 
				       {Orig_Dash#dash_line.center_point, 
					Orig_Dash#dash_line{
					  dash_after = Head_Dash#dash_line.center_point}
				       }),
		    
			    %%		    io:format("ORIGINAL :~p~nCONNECTION : ~p", [Orig_Dash, Connected_Dashes]),
			    map_functions:clean_ets_dashes(Orig_Dash#dash_line.dash_after),
			    map_functions:insert_dashes(Connected_Dashes)
		    end,
 		    New_CarPos = map_functions:move_point(Car_Pos, Correction),
		    New_CarTail = map_functions:move_point(Car_Tail, Correction),
		    New_CarHeading = steering:getAng(New_CarTail, New_CarPos),
 		    gen_server:cast(vehicle_data, {correct_position, New_CarPos , New_CarHeading}),
		    {noreply, State#state{mode = recording , debug = off,
					  last_dashes = map_functions:get_last_dashes()}}
 	    end
    end;

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

