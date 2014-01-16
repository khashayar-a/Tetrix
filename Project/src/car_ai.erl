-module(car_ai).

%% API
-export([start/2, init/2]).

%% Internal exports
%-export([calculate/1]).

-define(SERVER, ?MODULE).
-define(RPI, 'node2@192.168.3.150').

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------

% @doc
% Starts the module
start(NodeList, Time) ->
    Pid = spawn(?SERVER, init, [NodeList, Time]),
    {ok, Pid}.

% @doc
% Calculates speed and steering, and sends it to cunit server

%%--------------------------------------------------------------------
% Callback Definitions 
%%--------------------------------------------------------------------

init(NodeList,Time) ->
    io:format("CAR AI ~p ~n", [NodeList]),
    %% timer:sleep(2000),
    calculate(NodeList,Time),
    ok.

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

calculate(NodeList,{Pic,Time}) ->
    %% Get car position from vehicle data, in form of {X, Y}
    Car_Position = vehicle_data:car_position(), 
    Car_Heading = vehicle_data:car_heading(),

    %% Len = length(Node_List),
    %% P1 = hd(Node_List),
    %% P2 = lists:nth(round(Len/2), Node_List),
    %% P3 = lists:nth(Len, Node_List),
    
    [P1,P2,P3] = map_functions:get_nodes_ahead(NodeList , []),
    %% [P1,P2,P3] = map_gen:node_ahead(Car_Position, Car_Heading),
    
    


    %% Get 3 node lists ahead, i.e. Node1 = {5,6}, etc              
    %% {P1,P2,P3} = map_gen:node_ahead(Car_Position), 
    %% io:format("Nodes ahead"),
    %% TODO: calculate heading and speed
    Steering = steering:calculate(P1, P2, P3, Car_Position, Car_Heading),
    
    %% io:format("STEERING ANGLE : ~p~n" , [Steering]),

    %% send desired speed to cunit 
    %% TODO: dummy values
 %%   cunit:setSpeed(2),
    
    %% send desired steering to cunit
    %% TODO: dummy values    ok.
    %% cunit:setSteering(round((Steering * 180.0) / math:pi())),
    Angle = round((Steering * 180.0) / math:pi()),
    Speed = 2,
%%    gen_server:cast({hardware, ?RPI}, {drive, Speed, Angle, {Pic,Time}}),
%%    io:format("Steering: ~p~n", [Angle]),
%%    Speed = cunit:getAccelSpeed(),
%%    Heading = cunit:getHeading(),
%%    {ok, Log} = file:open("Log.txt", [read, append]),
%%    {Mega, Sec, Micro} = erlang:now(),
%%    io:fwrite(Log, "~p,~p,~p,~p,~p,~p,~p,~n", [Mega, Sec, Micro, Speed, Heading, Steering, round((Steering*180.0)/ math:pi())]),
%%    file:close(Log).    
    
    case {Angle < 26, Angle > -26} of
        {true, true} ->
            Third = integer_to_list( round(Angle * 1.67) + 90 ) ;
        {true,false} ->
            Third = integer_to_list( round(-25 * 1.67) + 90 ) ;
        {false, true} ->
            Third = integer_to_list( round(25*1.67) + 90)
    end,
    Second = "1", %% integer_to_list(State#state.speed),
    Serial = "tetrixSerial "++ Third ++ " 90 " ++ Second,
    os:cmd(Serial),


%%    DiffTime = diff_time(Time, erlang:now()),
%%    io:format("Picture : ~p -> CARAI ~p ~n", [Pic, DiffTime]), 
%%    timer:sleep(20),
 
  %%  calculate().
    done.
%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

diff_time({_,S1,MiS1},{_,S2,MiS2}) -> 
    ((S2-S1) * 1000000) + (MiS2 - MiS1).
