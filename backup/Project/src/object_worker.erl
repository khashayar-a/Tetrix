-module(object_worker).

%% API
-export([start/0, init/0]).

%% Internal exports

-define(SERVER, ?MODULE).

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
    object_detection:start_link(),
    loop().

loop() ->
    timer:sleep(10),
    List = fetch_data(),
    %% F = fun(X) -> gen_server:cast(object_detection, {save_data,X}) end,
    %% lists:foreach(F,List),
	    
    loop().

%%--------------------------------------------------------------------
% Internal functions Definitions 
%%--------------------------------------------------------------------

fetch_data() ->    
    object_detection:get_data().

%% Console print outs for server actions (init, terminate, etc) 
say(Format, Data) ->
    io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).
