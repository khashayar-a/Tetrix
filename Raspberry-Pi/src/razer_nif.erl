-module(razer_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
     erlang:load_nif("./ebin/razer_nif", 0).

get_byte() ->
    ?NIF_STUB.

deinit_razer() ->
    ?NIF_STUB.


%% INTERNAL
