-module(lidar_nif).

-compile(export_all).

-define (SERVER, ?MODULE).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./ebin/lidar_nif", 0).

read() ->
    {X, Y} = get_lidar(),
    write(X,Y).

write([],[]) ->
    ok;

write([H|T],[X|S]) ->
    
    file:write_file("/home/pi/data.txt", io_lib:fwrite("~p~n", [{H,X}]), [append]),
    write(T,S).

loop() ->
    {X,Y} = get_lidar(),
    io:format("X: ~p~n", [X]),
    io:format("Y: ~p~n", [Y]),
    timer:sleep(100),
    loop().

get_lidar() ->
    ?NIF_STUB.

deinit_lidar() ->
    ?NIF_STUB.

%% INTERNAL
