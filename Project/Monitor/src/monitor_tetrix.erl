-module(monitor_tetrix).

-behaviour(gen_server).

%% API
-export([start_link/0, init_loop/1, carHeading_and_carPOS/0]).

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

    %register(monitor_tetrix, self()), 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Function for making a handle_call, that returns the cars current
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

% Receives CarHeading and CarPos from the main Tetrix app node
handle_info({ok, {CarHeading, CarPOS}}, State) ->
  %io:format("tetrix app is available!\n",[]),
  {noreply, State#state{heading = CarHeading, currentPOS = CarPOS }};

% Receives initial CarHeading and CarPos from the main Tetrix app node
handle_info({initial_POS_heading, From}, State) ->
  %io:format("tetrix app is available!\n",[]),

  % Send current position and heading to vehicle data
  From ! {heading_POS_from_monitor, {State#state.heading, State#state.currentPOS  }},
  {noreply, State};


handle_info({initial_data, {CarHeading, CarPOS}}, State) ->
  %io:format("tetrix app is available!\n",[]),
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
  %register(monitor_tetrix, self()), 
  proc_lib:start_link(?SERVER, init_loop, [self()]),
  {ok, #state{heading = math:pi()/2, currentPOS = {100,200}}}.

init_loop(Parent) ->
  proc_lib:init_ack(Parent, {ok, self()}),
  loop(Parent).

loop(PID)->
  case {rpc:multicall([node1@odroidx2], erlang, is_alive, []) } of

    % no connections
    { {[], _} } ->
         case net_kernel:connect_node(node1@odroidx2) of
            false ->
              %io:format("false: no connections at all\n",[]),
              timer:sleep(500),
              %case startup_tetrix() of
              %  ok ->
              %    net_kernel:connect_node(node1@odroidx2)
              %end,
              timer:sleep(500);
            true ->
              %io:format("just connected to tetrix app!\n")
              ok
         end,
         loop(PID);

    % tetrix app is up
    { {[true], _} } ->
        {shell, node1@odroidx2} ! {check_availability, PID},
        timer:sleep(500),
        loop(PID)

  end.

startup_tetrix() ->
  os:cmd(".././init_tetrix"), 
  RawAnswer = os:cmd("./src/tetrix_available.sh"),  
  Answer = string:strip(RawAnswer,right,$\n),

  case Answer of
    "0" ->
        %io:format("looping, trying to start tetrix app", []),
        startup_tetrix();
    "1" ->
        ok 
  end. 

% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
