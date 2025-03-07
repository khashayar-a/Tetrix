-module(hardware).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(ODROID, 'node1@192.168.3.160').

-record(state, {hal = 0 , heading , prev_heading=0 , data}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
    razer_reader:start(),
    hal_reader:start(),
    {ok, #state{hal = 0 ,heading =0 , prev_heading= 0}}.

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
handle_call(initial_position, _From, State) ->
    Reply = State#state.data,
    {reply, Reply, State};
handle_call(initial_heading, _From, State) ->
    Reply = State#state.heading,
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
handle_cast({hal,Value}, State) ->
    Data = calculate_pos(State#state.hal, Value, State#state.prev_heading, State#state.heading),
    gen_server:cast({vehicle_data, ?ODROID} ,  {update_position, Data}),
    {noreply, State#state{prev_heading = State#state.heading, hal = Value, data = Data}};
%handle_cast({razer, Value} , State) ->
%    {noreply, State#state{heading = Value}};
handle_cast({angle, Value} , State) ->
    {noreply, State#state{heading = Value}};
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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

calculate_pos(PrevHal, CurrHal, PrevHeading, CurrHeading)->

    NewHal = (CurrHal - PrevHal)*(1000/57),

    case (NewHal < 0) of
	true ->
	    DeltaDistance = NewHal + 1000;
	false ->
	    DeltaDistance = NewHal
    end,

    Angle = CurrHeading - PrevHeading,

    DeltaHeading = normalized((Angle*math:pi()/180))*(180/math:pi()),

    PosX = DeltaDistance * ( math:cos( (CurrHeading * math:pi()/180) )),
    
    PosY = DeltaDistance * ( math:sin( (CurrHeading * math:pi()/180) )),
    
    {PosX, PosY, DeltaHeading}.

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
