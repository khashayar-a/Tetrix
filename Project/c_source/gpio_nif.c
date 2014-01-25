#include "/usr/lib/erlang/usr/include/erl_nif.h"
#include "gpio_functions.h"

//------------------------------------------------------------------------------
// NIF callbacks
//------------------------------------------------------------------------------


/* Function that returns the current movement 
*/
static ERL_NIF_TERM get_movement(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int movement = get_movement_data();
    return enif_make_int(env,movement);
}

/* Initializes the GPIO pins by mapping them to local space, and begins reading
 * pins 27, 29, and 31 in a  loop 
 */
static ERL_NIF_TERM init_gpio_pins(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  initialize_pin_reading();

  return enif_make_atom(env, "ok");
 
}


/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"init_gpio_pins", 0, init_gpio_pins},
    {"get_movement", 0, get_movement}
  };

ERL_NIF_INIT(gpio_nif,nif_funcs,NULL,NULL,NULL,NULL)

