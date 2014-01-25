-module(gpio_nif).

-compile(export_all).

-define(NIF_STUB, exit(nif_library_not_loaded)).

-on_load(init/0).

init() ->
    erlang:load_nif("./ebin/gpio_nif",0).
    %erlang:load_nif("./../ebin/gpio_nif", 0).

init_gpio_pins() ->
    ?NIF_STUB.

get_movement() ->
    ?NIF_STUB.

start() ->
    erlang:load_nif("./ebin/gpio_nif",0).
    %erlang:load_nif("./../ebin/gpio_nif",0).
