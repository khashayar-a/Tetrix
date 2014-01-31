%%%-------------------------------------------------------------------
%%% @author Tetrix <tetrx@localhost.localdomain>
%%% @copyright (C) 2014, Tetrix
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2014 by Tetrix <tetrix@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(object_detection).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, get_data/0, match_spec/3, clean_ets/0]).

-define(SERVER, ?MODULE).
-define(NIF_STUB, exit(nif_library_not_loaded)).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    ets:new(object_data, [set, named_table]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    X = lidar_nif:init(),
    case X of 
	-1 ->
	    "Failed To Load Lidar !!";
	_ ->
	    io:format("Lidar Initialized~n", []),
	    {ok, #state{}}
    end.

get_data() ->
    List = gen_server:call(?SERVER, get_data),
    clean_ets(),
    store_data(List).

store_data([]) ->
    ok;

store_data([{X,Y}|Tail])->
    ets:insert(object_data,{{(round(X/10))*10,(round(Y/10))*10},{X,Y}}),
    store_data(Tail).
	
clean_ets() ->
    %{X,Y} = vehichle_data:car_position(),
    ets:select_delete(object_data, match_spec(0,0,1000)).


match_spec(Cx, Cy, R) ->    
    [{{{'$1','$2'},'_'},
      [{'<',{'+',{'*',{'-','$1',{const,Cx}},
                  {'-','$1',{const,Cx}}},
             {'*',{'-','$2',{const,Cy}},{'-','$2',{const,Cy}}}},
        {'*',{const,R},{const,R}}}],
      [true]}].
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_data, _From, State) ->
    Reply = lidar_nif:get_lidar(),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("MESSAGE COMMING : ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("MESSAGE INFO COMMING : ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
