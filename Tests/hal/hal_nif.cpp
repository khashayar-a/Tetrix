#include "/usr/lib/erlang/usr/include/erl_nif.h"
#include "hal_functions.hpp"
#include "iostream"
#include <ostream>
#include <sstream>
#include <string>
#include <sys/time.h>

using namespace std;


static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{

  //Initializes the serial port
  int result = init();
  cout << "HAL LOADING " << result << endl;

  if (result != -1)
    return 0;
  else
    return 1;
}

/* Retrive a char from serial port
 */
static ERL_NIF_TERM get_byte(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

  int c = read_byte();
  return enif_make_int(env, c); 
  //  return enif_make_atom(env, "ok");
  
}

static ERL_NIF_TERM deinit_hal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  deinit();
  return enif_make_atom(env, "ok");
 
}

/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"get_byte", 0, get_byte},
    {"deinit_hal", 0, deinit_hal}
  };

ERL_NIF_INIT(hal_nif,nif_funcs,load,NULL,NULL,NULL)

