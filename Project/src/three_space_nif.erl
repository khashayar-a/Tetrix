-module(three_space_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./ebin/three_space_nif", 0).

get_heading() ->
    ?NIF_STUB.

deinit_3space() ->
    ?NIF_STUB.

switch_compass(_) ->
    ?NIF_STUB.

%% INTERNAL
