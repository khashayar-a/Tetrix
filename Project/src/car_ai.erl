-module(car_ai).

%% API
-export([start/0, init/0]).

%% Internal exports
-export([calculate/1]).

-define(SERVER, ?MODULE).
-define(RPI, 'node2@192.168.3.150').

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start() ->
    Pid = spawn(?SERVER, init, []),
    {ok, Pid}.

% @doc
% Calculates speed and steering, and sends it to cunit server

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init() ->
    io:format("CAR AI INITNT~n", []),
    
    P = pcbnif:start_pcb(),
    pcbnif:set_speed(10,P),
    timer:sleep(5000),
    calculate(P),
    ok.

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

calculate(Pcb_Address) ->
    %% RemoteOn = pcbnif:get_remoteStatus(Pcb_Address),
    %%   %%io:format("REMOTE STATUS : ~p~n" , [RemoteOn]),
    %% case RemoteOn of
    %%  	1 ->
    %%   	    pcbnif:set_remoteLight(1, Pcb_Address),
    %%   	    UpDown= pcbnif:get_remoteY(Pcb_Address),
    %%   	    RightLeft=pcbnif:get_remoteX(Pcb_Address),
    %%   	    case  RightLeft of
    %%  	    	1 -> Ang=125;
    %%  		2 -> Ang=55;
    %%   		_Else -> Ang=90
    %%   	    end,
    %% 	    pcbnif:set_angle(Ang, Pcb_Address),
    %%   	    pcbnif:set_speed(90, Pcb_Address),
    %%  	    pcbnif:set_direction(UpDown, Pcb_Address),
    %%   	    timer:sleep(1),
    %%  	    calculate(Pcb_Address);
    %%  	_Else ->
    %%   	    %% Get car position from vehicle data, in form of {X, Y}
    %%   	    pcbnif:set_remoteLight(0, Pcb_Address),
	    
	    
	    

    Car_Position = vehicle_data:car_position(), 
    Car_Heading = vehicle_data:car_heading(),
    	%% Len = length(Node_List),
    	%% P1 = hd(Node_List),
    	%% P2 = lists:nth(round(Len/2), Node_List),
    	%% P3 = lists:nth(Len, Node_List),
       	%% [P1,P2,P3] = map_functions:get_nodes_ahead(NodeList , []),



    case map_gen:node_ahead(Car_Position, Car_Heading) of
      	not_found ->
      	    io:format("We are lost and fucked~p~n", [{Car_Position, Car_Heading}]),
      	    pcbnif:set_direction(4, Pcb_Address);
      	[P1,P2,P3] ->
      	    Real_Car_Pos = calculate_actual_pos(Car_Position, Car_Heading),
      	    Steering = steering:calculate(P1, P2, P3, Real_Car_Pos, Car_Heading),
      	    Angle = round((Steering * 180.0) / math:pi()),	    
      	    case {Angle < 26, Angle > -26} of
     		{true, true} ->
      		    Third = round(Angle * 2.3) + 90;
     		{true,false} ->
      		    Third = round(-25 * 2.3) + 90;
      		{false, true} ->
      		    Third = round(25 * 2.3) + 90
      	    end,
%%      	    io:format("DRIVING WITH ~p ~n" , [Third]),
      	    pcbnif:set_angle(Third, Pcb_Address),
      	    pcbnif:set_direction(1, Pcb_Address)
    end,
    
      


    	%% Get 3 node lists ahead, i.e. Node1 = {5,6}, etc              
    	%% {P1,P2,P3} = map_gen:node_ahead(Car_Position), 
    	%% io:format("Nodes ahead"),
    	%% TODO: calculate heading and speed
    	%%    Steering = steering:calculate(P1, P2, P3, Car_Position, Car_Heading),
    
    	%% io:format("STEERING ANGLE : ~p~n" , [Steering]),

    	%% send desired speed to cunit 
    	%% TODO: dummy values
    	%%   cunit:setSpeed(2),
    
    	%% send desired steering to cunit
    	%% TODO: dummy values    ok.
    	%% cunit:setSteering(round((Steering * 180.0) / math:pi())),
    	%%    Angle = round((Steering * 180.0) / math:pi()),
    	%%    Speed = 2,
    	%%    gen_server:cast({hardware, ?RPI}, {drive, Speed, Angle, {Pic,Time}}),
    	%%    io:format("Steering: ~p~n", [Angle]),
    	%%    Speed = cunit:getAccelSpeed(),
    	%%    Heading = cunit:getHeading(),
    	%%    {ok, Log} = file:open("Log.txt", [read, append]),
    	%%    {Mega, Sec, Micro} = erlang:now(),
    	%%    io:fwrite(Log, "~p,~p,~p,~p,~p,~p,~p,~n", [Mega, Sec, Micro, Speed, Heading, Steering, round((Steering*180.0)/ math:pi())]),
    	%%    file:close(Log).    
    
    	%% case {Angle < 26, Angle > -26} of
    	%%     {true, true} ->
    	%%         Third = integer_to_list( round(Angle * 1.67) + 90 ) ;
    	%%     {true,false} ->
    	%%         Third = integer_to_list( round(-25 * 1.67) + 90 ) ;
    	%%     {false, true} ->
    	%%         Third = integer_to_list( round(25*1.67) + 90)
    	%% end,
    	%% Second = "1", %% integer_to_list(State#state.speed),
    	%% Serial = "tetrixSerial "++ Third ++ " 90 " ++ Second,
    	%% os:cmd(Serial),


	    %%    DiffTime = diff_time(Time, erlang:now()),
	    %%    io:format("Picture : ~p -> CARAI ~p ~n", [Pic, DiffTime]), 
    timer:sleep(1),
    calculate(Pcb_Address) .  
   %% end.
%% done.
%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

diff_time({_,S1,MiS1},{_,S2,MiS2}) -> 
    ((S2-S1) * 1000000) + (MiS2 - MiS1).


calculate_actual_pos({X,Y} , Heading) ->
    Tx = math:cos(Heading + math:pi()) * 72 + X,
    Ty = math:sin(Heading + math:pi()) * 72 + Y,
    {Tx,Ty}.
