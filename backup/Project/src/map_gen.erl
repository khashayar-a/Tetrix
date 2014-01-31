-module(map_gen).

-behaviour(gen_server).

%% Internal functions
-export([say/2]).
-compile(export_all).

%% API
-export([start_link/0, node_ahead/2, add_frame/4, road_side/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {node_ahead, road_side, frame_data, matrix_id, camera_matrix, mode, last_dashes , debug, intersection, intersection_node, over_take}).
-record(dash_line, {center_point, box, points, area, dash_before, dash_after, intersection}).

-include("../include/offsetCalculation.hrl").

init([]) ->
    say("init", []),
    
    {ok, ID} = ets:file2tab("include/undistort.txt"),
    
%%    ets:new(dash_lines, [set, named_table]),
    ets:file2tab("include/Jan29-4"),
    %% Dummy values for the state 
    Car_Pos =  vehicle_data:car_position(),
    Car_Heading = vehicle_data:car_heading(),
    Dashes_Ahead = map_functions:get_dashes_ahead(Car_Pos, Car_Heading),

    {ok, #state{road_side = right, node_ahead = {{0,0},{0,0},{0,0}},
		matrix_id = ID , mode = following, 
		last_dashes = Dashes_Ahead ,
		debug = on
		}}.

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% @doc
% Argument is a tuple containing X and Y coords, i.e. Car_Pos = {5,6}
node_ahead(Car_Pos , Car_Heading) ->
    gen_server:call(
      ?SERVER,
      {node_ahead, Car_Pos, Car_Heading}).

nodes(Car_Pos , Car_Heading) ->
    gen_server:call(?SERVER, get_nodes).

stop() ->
    gen_server:cast(?SERVER, stop).

over_take(X) ->
    gen_server:cast(?SERVER, {over_take, X}).

% @doc
% Adds frame data from image processing. Arguments are Points detected, and Car
% position 
add_frame(Points, Lane_ID, Car_Pos, Time) ->
    gen_server:cast(
      ?SERVER,
      {add_frame, {{Points, Lane_ID}, Car_Pos}, Time}).

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
    T = map_functions:separate(ets:tab2list(dash_lines), [], N),
    io:fwrite(Log, "~p~n" , [T]),
    file:close(Log).

last_dashes() ->
        gen_server:call(?SERVER, last_dashes).

%%--------------------------------------------------------------------
% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call({node_ahead,{CarX,CarY} , Heading}, _From, State = #state{mode = stop}) ->
    {reply, not_found, State};

handle_call({node_ahead,{CarX,CarY} , Heading}, _From, State = #state{intersection = active}) ->

    NodeList = 
	map_functions:get_dashes_ahead(3, State#state.intersection_node),
    io:format("Driving in intersection with : ~p ~n Car On ~p , ~p~n", 
	      [NodeList, {CarX,CarY}, Heading]),
    Reply = map_functions:get_nodes_ahead(NodeList),
    {reply, Reply, State};

handle_call({node_ahead,{CarX,CarY} , Heading}, _From, State = #state{over_take = active}) ->
    % TODO: generate node ahead with {X,Y} values, using dummy values
    %% query ets for node ahead
%%    [D1,D2,D3|_] = State#state.last_dashes,

    case map_functions:find_closest_dash_ahead({CarX,CarY}, 1000) of
	not_found ->
	    Reply = not_found;
	Dash ->
	    case Dash#dash_line.dash_before of 
		undef ->
		    NodeList = 
			map_functions:get_dashes_ahead(3, Dash#dash_line.center_point);
		Before ->
		    NodeList = map_functions:get_dashes_ahead(3, Before)
	    end,
%%	    io:format("HERE AT : ~p, ~p DRIVING ON PATH OF THESE NODES :~p~n", [ {CarX,CarY}, Heading, NodeList]),
	    Reply = map_functions:get_opposite_nodes_ahead(NodeList)
%%	    io:format("DRIVING ON NODES : ~p~n",[Reply])
    end,
    {reply, Reply, State};
handle_call({node_ahead,{CarX,CarY} , Heading}, _From, State) ->
    % TODO: generate node ahead with {X,Y} values, using dummy values
    %% query ets for node ahead
%%    [D1,D2,D3|_] = State#state.last_dashes,

    case map_functions:find_closest_dash_ahead({CarX,CarY}, 1000) of
	not_found ->
	    Reply = not_found;
	Dash ->
	    case Dash#dash_line.dash_before of 
		undef ->
		    NodeList = 
			map_functions:get_dashes_ahead(3, Dash#dash_line.center_point);
		Before ->
		    NodeList = map_functions:get_dashes_ahead(3, Before)
	    end,
%%	    io:format("HERE AT : ~p, ~p DRIVING ON PATH OF THESE NODES :~p~n", [ {CarX,CarY}, Heading, NodeList]),
	    Reply = map_functions:get_nodes_ahead(NodeList)
%%	    io:format("DRIVING ON NODES : ~p~n",[Reply])
    end,
    {reply, Reply, State};

handle_call(get_nodes, _From, State) ->
    Reply = State#state.road_side,
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

handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos = {CarX,CarY}, Car_Tail, Car_Heading}}, Time}, 
	    State = #state{mode = recording}) ->
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    
%%    Temp_Dashes = clear_far(Car_Pos, All_Dashes, []),
    Correction = map_functions:calculate_correct_pos(Temp_Dashes , State#state.last_dashes),
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
	    Estimated_Dashes =
	     	map_functions:generate_estimated_dashes(State#state.last_dashes),  
	    Estimated_Correction = 
	     	map_functions:calculate_estimated_correction(Temp_Dashes, Estimated_Dashes),
	    Last_Dash = map_functions:get_last(State#state.last_dashes),
	    Temp_Head_Dash = hd(Temp_Dashes),
	    Distance = map_functions:getDistance(Last_Dash#dash_line.center_point,
						  Temp_Head_Dash#dash_line.center_point),

	    case {Distance > 500, Estimated_Correction} of
		{false, _} ->
		    ok;
		{_, not_found} ->
		    ok;
		{true, {Dashes_Needed, Corresponding_Dash, {Offset, Delta_Angle}}} ->
		    io:format("Estimated Found: ~p~n", [Estimated_Correction]),
		    Moved_Dashes = map_functions:move_dashes(Dashes_Needed, 
							     {Corresponding_Dash#dash_line.center_point,
							      {Offset,Delta_Angle}}, []),
		    Connected_Dashes = map_functions:connect_dashes(Moved_Dashes,
								    Last_Dash#dash_line.center_point,
								    []),
		    Head_Dash = hd(Connected_Dashes),
		    ets:insert(dash_lines, 
			       {Last_Dash#dash_line.center_point, 
				Last_Dash#dash_line{
				  dash_after = Head_Dash#dash_line.center_point}
			       }),
		    
		    map_functions:insert_dashes(Connected_Dashes),
		    New_Car_Pos = map_functions:move_point(Car_Pos, 
							   {Corresponding_Dash#dash_line.center_point,
							    {Offset,Delta_Angle}}),
		    New_Car_Heading = Car_Heading + Delta_Angle,
		    gen_server:cast(vehicle_data, 
				    {correct_position, New_Car_Pos , New_Car_Heading})
	    end;
	
	{Dashes_Needed, Corresponding_Dash, {Offset = {Ox,Oy}, Delta_Angle}} ->
	    Movement = map_functions:getDistance({0,0}, Offset),
	    case Movement > 2 of
		false ->
		    ok;
		true ->
		    Moved_Dashes = map_functions:move_dashes(Dashes_Needed, 
							     {Corresponding_Dash#dash_line.center_point,
							      {Offset,Delta_Angle}}, []),
	
		    Connected_Dashes = map_functions:connect_dashes(Moved_Dashes,
								    Corresponding_Dash#dash_line.dash_before,
								    []),
		    case Corresponding_Dash#dash_line.dash_before of
			undef ->
			    ok;
			Point ->
			    Head_Dash = hd(Connected_Dashes),
			    Before_Dash = map_functions:ets_lookup(Point),
			    ets:insert(dash_lines, 
				       {Before_Dash#dash_line.center_point, 
					Before_Dash#dash_line{
					  dash_after = Head_Dash#dash_line.center_point}
				       })
		    end,
		    map_functions:clean_ets_dashes(Corresponding_Dash#dash_line.center_point),
		    map_functions:insert_dashes(Connected_Dashes),
		    New_Car_Pos = {CarX+Ox, CarY+Oy},
%%		    New_Car_Pos = map_functions:move_point(Car_Pos, 
%%							   {Corresponding_Dash#dash_line.center_point,
%%							    {Offset,Delta_Angle}}),
		    New_Car_Heading = Car_Heading + 0,%%Delta_Angle,
		    gen_server:cast(vehicle_data, 
				    {correct_position, New_Car_Pos , New_Car_Heading}),
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



handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos, Car_Tail, Car_Heading}} , Time}, 
	    State = #state{mode = start}) ->
    
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    NewDashes = map_functions:connect_dashes(Temp_Dashes, undef, []), 
    io:format("START MODE ~p , ~p ~n" , [Car_Pos, Car_Heading]),
    io:format("Temp DASHES : ~p~n" , [Dashes]),
    io:format("NEW DASHES : ~p~n" , [NewDashes]),
    map_functions:insert_dashes(NewDashes),
    {noreply, State#state{mode = recording, last_dashes = map_functions:get_last_dashes(),
			  debug = on}};


handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos = {CarX,CarY}, Car_Tail, Car_Heading}}, {Pic,Time}}, 
	    State = #state{mode = following}) ->
    StartTime = erlang:now(),
    
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {Car_Pos, Car_Heading}, []),
    
    Correction = map_functions:calculate_correct_pos(Temp_Dashes , State#state.last_dashes),
    
    case Correction of 
	not_found ->
	    Intersection = State#state.intersection,
	    Intersection_Node = State#state.intersection_node,
	    case State#state.debug of
		on ->
		    io:format("Correction NOT FOUND : ~p~n", [Temp_Dashes]);	
		_ ->
		    ok
	    end,
	    Dashes_Ahead = State#state.last_dashes;
	{Dashes_Needed, Corresponding_Dash, {Offset = {Ox,Oy}, Delta_Angle}} ->
	    case Corresponding_Dash#dash_line.intersection of 
		start ->
		    io:format("INTERSECTION ACTIVATED with : ~p ~n",[Corresponding_Dash#dash_line.dash_after]),
		    Intersection = active,
		    Intersection_Node = Corresponding_Dash#dash_line.dash_after;
		finish ->
		    io:format("INTERSECTION Terminated with : ~p ~n",[Corresponding_Dash]),
		    Intersection = undef,
		    Intersection_Node = undef;
		_ ->
		    Intersection = State#state.intersection,
		    Intersection_Node = State#state.intersection_node
	    end,
	    case State#state.debug of
		on ->
		    io:format("Correction FOUND : ~p~n", [Correction]);	
		_ ->
		    ok
	    end,
	    Movement = map_functions:getDistance({0,0}, Offset),
	    case Movement > -1 of
		false ->
		    Dashes_Ahead = State#state.last_dashes;
		true ->
		    New_Car_Pos = {CarX+Ox,CarY+Oy},%% map_functions:move_point(Car_Pos, 
		    %%			   {Corresponding_Dash#dash_line.center_point,
		    %%				    {Offset,Delta_Angle}}),
		    New_Car_Heading = Car_Heading +  0 ,%%Delta_Angle,
%%		    Dashes_Ahead = map_functions:get_dashes_ahead(New_Car_Pos, New_Car_Heading),
		    Dashes_Ahead = map_functions:get_dashes_ahead(5, 
								  Corresponding_Dash#dash_line.center_point),
		    
%%		    car_ai:start(NodeList, {Pic, Time}),
%%		    DiffTime = diff_time(Time, erlang:now()),
%%		    ThisTime = diff_time(StartTime, erlang:now()),
%%		    io:format("Picture : ~p -> Position ~p -> THIS : ~p~n", [Pic, DiffTime, ThisTime]),    
		    gen_server:cast(vehicle_data, 
				    {correct_position, New_Car_Pos , New_Car_Heading})
%%		    gen_server:cast(vehicle_data, 
%%				    {update_initial, Delta_Angle})
	    end
    end,
    {noreply, State#state{mode = following, last_dashes = Dashes_Ahead, 
			  debug = off, intersection = Intersection, intersection_node = Intersection_Node}};	
		    

handle_cast({add_frame, {{Dashes, Line_ID}, {Car_Pos, Car_Tail, Car_Heading}} , Time}, 
	    State = #state{mode = backup}) ->
    
    Temp_Dashes = map_functions:translate_dashes(State#state.matrix_id, 
						 Dashes, {{0,0}, Car_Heading}, []),
    NewDashes = map_functions:connect_dashes(Temp_Dashes, undef, []), 

    case length(NewDashes) of
	2 ->
	    [Dash1, Dash2] = NewDashes,

	    [_,Mid_Point,_] = map_functions:find_mid_dash(Dash1#dash_line.points, Dash2#dash_line.points),

	    io:format("MiDPOINT BASE ON : ~p  -- ~p  -- ~p ~n", [Dash1#dash_line.points, Mid_Point, Dash2#dash_line.points]),
	    Temp_Map = map_functions:add_dashes(Dash1#dash_line.center_point, Mid_Point, 
				     Dash2#dash_line.center_point, 5);
	_ ->
	    [Dash1, Dash2, Dash3 | _] = NewDashes,
	    Temp_Map = map_functions:add_dashes(Dash1#dash_line.center_point, 
						Dash2#dash_line.center_point,
						Dash3#dash_line.center_point, 5)
    end,

    New_Map = map_functions:connect_dashes(Temp_Map, undef, []), 
    map_functions:ets_clear(),
    io:format("MAP GENERATED : ~n" , []),
    map_functions:insert_dashes(New_Map),
    gen_server:cast(vehicle_data, reset_position),
    {noreply, State#state{last_dashes = map_functions:get_last_dashes(),
			  debug = off}};
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
handle_cast({over_take, X}, State) ->
    {noreply, State#state{over_take = X} };
handle_cast(stop, State) ->
    {noreply, State#state{mode = stop} };
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




clear_far(Car_Pos, [Dash |T] , Buff) ->
    case map_functions:getDistance(Car_Pos, Dash#dash_line.center_point) > 1500 of
	false ->
	    clear_far(Car_Pos, T , Buff ++ [Dash]);
	_ ->
	    clear_far(Car_Pos, T , Buff)
    end;
clear_far(_,[],Buff) ->
    Buff.
	


diff_time({_,S1,MiS1},{_,S2,MiS2}) -> 
    ((S2-S1) * 1000000) + (MiS2 - MiS1).
