-module(tetrix_status).

%-behaviour(gen_server).

%% API
-export([start/0, init/1]).

%% gen_server callbacks
%-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
%	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
%-define(Port, 8091).

start()->
  %net_kernel:start(['node1@arch', longnames]),
  State = [],
  Pid = spawn(?SERVER, init, [State]),
  register(shell, Pid),
  {ok, Pid}.

init(State) ->
  net_kernel:start(['node1', shortnames]),
  loop().

loop()->
  receive
    {establish_connection, From} ->
        io:format("in the node1 loop",[]),
        From ! {origin, self()};
    {check_availability, From} ->
        io:format("rec'd random message in check_availability",[]),
        CarHeading = vehicle_data:car_heading(),
        CarPOS = vehicle_data:car_position(),
        From ! {ok, {CarHeading, CarPOS}};
    _ ->
        io:format("rec'd random message in tetrix_status",[])
  end,
  loop().

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
