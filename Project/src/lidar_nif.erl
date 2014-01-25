-module(lidar_nif).

-compile(export_all).

-define (SERVER, ?MODULE).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./lidar_nif", 0).

read() ->
    List = get_lidar(),
    write(List).

write([]) ->
    ok;

write([H|T]) ->    
    file:write_file("/home/tetrix/lidar.txt", io_lib:fwrite("~p~n", [H]), [append]),
    write(T).

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
