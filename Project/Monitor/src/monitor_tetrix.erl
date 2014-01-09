-module(monitor_tetrix).

-behaviour(gen_server).

%% API
%e-xport([start/0]).
-export([start_link/0, init_loop/1, carHeading_and_carPOS/0]).

%% Internal exports
%-export([init/1, startup_tetrix/0]).

% server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
   terminate/2, code_change/3]).


-define(SERVER, ?MODULE). 

-record(state, {heading, currentPOS}).

%%--------------------------------------------------------------------
% API Function Definitions 
%%--------------------------------------------------------------------


% starts server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% function for making a handle_call, that returns the cars current
%% current heading and position, returning in the format of {0, {0,0}}
carHeading_and_carPOS() ->
    gen_server:call(?SERVER, heading_position).

%% Server Function Definitions
%%--------------------------------------------------------------------

handle_call(heading_position, _From, State) ->
    Reply = {State#state.heading, State#state.currentPOS}, 
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------

handle_info({ok, {CarHeading, CarPOS}}, State) ->
  io:format("tetrix app is available!\n",[]),
  {noreply, State#state{heading = CarHeading, currentPOS = CarPOS }};

handle_info(_Info, State) ->
    % Unexpected messages, i.e. out-of-band
    error_logger:info_msg("Unexpected message:~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [ _OldVsn, State, _Extra]),
    {ok, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating:~p~n", [?MODULE]),

    ok.


%% Internal functions
%%--------------------------------------------------------------------

init([]) ->
  %net_kernel:start(['node2', shortnames]),
%  loop(self()),
  proc_lib:start_link(?SERVER, init_loop, [self()]),
  {ok, #state{heading=0, currentPOS = {0,0}}}.

init_loop(Parent) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent).

loop(State)->
  check_r_pi(),
  case {rpc:multicall(['node1@192.168.3.160'], erlang, is_alive, []),
          rpc:multicall(['node2@192.168.3.150'], erlang, is_alive, [])} of

    % no connections
    {{[], _}, {[], _ } } ->
         case net_kernel:connect_node('node1@192.168.3.160') of
            false ->
              io:format("false: no connections at all\n",[]),
              timer:sleep(500),
              %timer:sleep(1000),
              case startup_tetrix() of
                ok ->
                  net_kernel:connect_node('node1@192.168.3.160')
              end,
              timer:sleep(500);
            true ->
              io:format("just connected to tetrix app!\n")
         end,
         loop(State);

    % tetrix app is down, monitor_tetrix_pi is up
    { {[],_}, {[true],_} } ->
         io:format("tetrix app is down, monitor_tetrix_pi is up\n",[]),
         %timer:sleep(1000),
         % do something to start up tetrix_app 
         case startup_tetrix() of
            ok ->
              net_kernel:connect_node('node1@192.168.3.160')
         end,
         timer:sleep(500),
         loop(State);

    % tetrix app is up, monitor_tetrix_pi is down 
    {{[true], _}, {[], _} } ->
        io:format("tetrix app is up, monitor_tetrix_pi is down\n",[]),
        timer:sleep(500),
        % do something to start up monitor_tetrix_pi 
        %os:cmd("ssh pi@192.168.3.150
        %    '/home/pi/Tetrix/Raspberry-Pi/Monitor/init_monitor';tetrix"),
        loop(State);


    % all nodes are up
    { {[true],_}, {[true], _} } ->

        io:format("all nodes are up!\n",[]),
        {shell, 'node1@192.168.3.160'} ! {check_availability, State},
        %State ! {check_availability, State},
        timer:sleep(500),
        loop(State)
  end.

startup_tetrix() ->
  os:cmd(".././init_tetrix"), 
  RawAnswer = os:cmd("./src/tetrix_available.sh"),  
  Answer = string:strip(RawAnswer,right,$\n),

  case Answer of
    "0" ->
        io:format("looping, trying to start tetrix app", []),
        startup_tetrix();
    "1" ->
        ok 
  end. 

check_r_pi() ->
  case rpc:multicall(['node2@192.168.3.150'], erlang, is_alive, []) of
    {[true], []} ->
      io:format("raspberry pi is up!\n",[]);
    {[], _} -> io:format("rasperry pi is down!\n",[])
  end.


% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

