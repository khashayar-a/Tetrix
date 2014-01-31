-module(vehicle_data).

-behaviour(gen_server).

%% API
-export([start_link/0, car_position/0, car_heading/0,  update_position/1, update_sensor/1, estimated/0, car_tail/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(RPI, 'node2@192.168.3.150').

-record(state, {car_position, heading , car_tail, mode = init, initial_heading,
	        estimated_car_position , estimated_heading , estimated_car_tail,
		sensor_data}).

init([]) ->
    say("init", []),
    %%net_kernel:connect_node(?RPI),
    %%case gen_server:call({hardware, ?RPI} , initial_position) of
%%	undefined ->
%%	    Position = {0,0};
%%	{_,X,Y} ->
%%	    Position = {X,Y}
%%    end,


%%  Code for having vehicle data retrieve car position and heading from the
%%  monitor
%%
%%    {Heading, Current_POS} = get_heading_POS_from_monitor(),
%%    io:format("***Data from monitor: Heading: ~p~n , Current POS: ~p~n",
%%          [Heading, Current_POS]),

    Position = {4857,3694},
    Heading = -0.5464177145377578, 
    %%0, %%math:pi() / 2, %%0, %gen_server:call({hardware, ?RPI} , initial_heading),
    Tail_Position = calculate_tail(Position, Heading), 
    {ok, #state{car_position = Position,  heading = Heading, car_tail = Tail_Position,  
		estimated_car_position = Position , estimated_heading = Heading, 
		estimated_car_tail = Tail_Position, sensor_data = {stuff, 0},
		initial_heading = 0 , mode = init}}.

%%--------------------------------------------------------------------
%% API Function Definitions 
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc
%% Retrieves current car position 
car_position() ->
    gen_server:call(?SERVER, car_position).

car_tail() ->
    gen_server:call(?SERVER, car_tail).

car_heading() ->
    gen_server:call(?SERVER, car_heading).

estimated() ->
    gen_server:call(?SERVER, get_estimated).

%% @doc
%% Updates current car position. Argument position is the X and Y the position
%% in tuple form, i.e. {1,2}
update_position(Position) ->
    gen_server:cast(
    ?SERVER,
    {update_position, Position}).

%% @doc
%% Updates sensor data
update_sensor(Data) ->
    gen_server:cast(
    ?SERVER,
    {update_sensor, Data}).


%%--------------------------------------------------------------------
%% gen_server Function Definitions
%%--------------------------------------------------------------------

handle_call(car_position, _From, State) ->
    Reply = State#state.estimated_car_position,
    {reply, Reply, State};

handle_call(car_tail, _From, State) ->
    Reply = State#state.estimated_car_tail,
    {reply, Reply, State};

handle_call(car_heading, _From, State) ->
    Reply = State#state.estimated_heading,
    {reply, Reply, State};

handle_call(get_estimated, _From, State) ->
    Reply = {State#state.estimated_heading, State#state.estimated_car_position},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast({update_position, {PosX, PosY, DeltaHeading}}, State) ->
    {OldPosX, OldPosY} = State#state.estimated_car_position,
    Oldheading = State#state.estimated_heading,
    %% io:format("New Heading: ~p~n", [{Oldheading , DeltaHeading}]),
    io:format("UPDATE POS: ~p~n", [{PosX, PosY, DeltaHeading}]),
    New_Heading = normalized(Oldheading + DeltaHeading),
    New_CarPos = {OldPosX + PosX, OldPosY + PosY},
    New_CarTail = calculate_tail(New_CarPos, New_Heading),
    {noreply, State#state{estimated_car_position = New_CarPos, 
			  estimated_heading = New_Heading , 
			  estimated_car_tail = New_CarTail}};
handle_cast({hal_moved, Hal}, State) ->
    {Dx,Dy} = calculate_pos(Hal, State#state.estimated_heading),
    {X,Y} = State#state.estimated_car_position,
    Car_Tail = calculate_tail({X+Dx, Y+Dy}, State#state.heading),
    {noreply, State#state{estimated_car_position = {X+Dx, Y+Dy}, 
			  estimated_car_tail = Car_Tail}};
handle_cast({update_sensor, Data}, State) ->
    {noreply, State#state{sensor_data = Data }};

handle_cast({correct_position, Position , Heading}, State) ->
%%    io:format("Position Changed from ~p To ~p  --- HEADING : ~p~n", 
%%	      [State#state.estimated_car_position, Position, State#state.estimated_heading]), 
    {noreply, State#state{car_position = Position , %%heading = Heading, 
			  car_tail = calculate_tail(Position , Heading),
			  %%estimated_heading = normalized(Heading),
			  estimated_car_position = Position, 
			  estimated_car_tail = calculate_tail(Position , Heading)}};
handle_cast(reset_position, State) ->
    {noreply, State#state{estimated_car_position = {0,0} ,
			  car_position = {0,0}}};
handle_cast({angle, Heading}, State) ->
%%    io:format("HEADING COMIN ~p~n", [Heading]),
    case State#state.mode of
	init ->
	    Initial = -0.5464177145377578 - Heading,
	    {noreply, State#state{estimated_heading = Heading + Initial, 
				  initial_heading = Initial, mode = run }};
	_ ->
	    {noreply, State#state{estimated_heading = Heading + State#state.initial_heading }}
    end;	    

handle_cast({update_initial, Delta}, State) ->
    {noreply, State#state{initial_heading = State#state.initial_heading + Delta }};

%%, estimated_car_position = Position
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    %% Unexpected messages, i.e. out-of-band
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

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).


calculate_tail({X,Y} , Heading) ->
    Tx = math:cos(Heading + math:pi()) * 20 + X,
    Ty = math:sin(Heading + math:pi()) * 20 + Y,
    {Tx,Ty}.


calculate_pos(NewHal, CurrHeading)->
    DeltaDistance = (NewHal)*(5.25),
    PosX = DeltaDistance * ( math:cos( CurrHeading )),
    PosY = DeltaDistance * ( math:sin( CurrHeading )),
    {PosX, PosY}.

normalized(Angle)->
    case {Angle > math:pi() , Angle < -math:pi()} of
        {true, _} ->
            NewAngle = normalize(Angle, (-2.0 * math:pi()) );
        {_,true} ->
            NewAngle = normalize(Angle, (2.0 * math:pi()) );
        _ ->
            NewAngle = Angle
    end,
    case abs(NewAngle) == math:pi() of
        true ->
            0.0;
        _ ->
            NewAngle
    end.

normalize(Angle, MyPI) ->
    case {Angle > math:pi() , Angle < -math:pi()} of
        {false,false} ->
            Angle;
        _ ->
            normalize(Angle+MyPI, MyPI)
    end.

get_heading_POS_from_monitor() ->
   {monitor_tetrix, node2@odroidx2 } ! {initial_POS_heading, self()},
   receive
      {heading_POS_from_monitor, {Heading, Current_POS} } ->
          {Heading, Current_POS} 
   end.

