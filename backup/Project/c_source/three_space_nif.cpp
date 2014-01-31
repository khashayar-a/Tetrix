#include "/usr/lib/erlang/usr/include/erl_nif.h"
#include "three_space_functions.hpp"
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
  cout << "3space initialized" << result << endl;

  if (result != -1)
    return 0;
  else
    return 1;
}

/* Retrive a char from serial port
 */
static ERL_NIF_TERM get_heading(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  
  float c = heading();
  return enif_make_double(env, c); 
   
}

static ERL_NIF_TERM deinit_3space(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){

  deinit();
  return enif_make_atom(env, "ok");
 
}

static ERL_NIF_TERM switch_compass(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  int flag;
  enif_get_int(env, argv[0], &flag);
  set_compass(flag);
  return enif_make_atom(env, "ok");
}


/* Nif function definitions */
static ErlNifFunc nif_funcs[] =
  {
    {"get_heading", 0, get_heading},
    {"switch_compass", 1, switch_compass},
    {"deinit_3space", 0, deinit_3space}
  };

ERL_NIF_INIT(three_space_nif,nif_funcs,load,NULL,NULL,NULL)

