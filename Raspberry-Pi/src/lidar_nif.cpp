#include <math.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

#include <vector>

#include "iostream"
#include <time.h>
#include "thread"

#include "/usr/lib/erlang/usr/include/erl_nif.h"
#include "iostream"
#include "time.h"
#include <ostream>
#include <sstream>
#include <string>
#include <sys/time.h>

#include <pthread.h>
#include <mutex>

using namespace std;

typedef struct {
  double x;
  double y;
} Point;

vector<Point> point_vector;
int tty_fd;
int lidar_x[360];
int lidar_y[360];

mutex mtx;

int init_lidar()
{
  struct termios tio;

  if (tcgetattr(tty_fd,&tio) < 0) 
    {
      cout << "get attr fail" << endl;
      return -1;
    }
 
  memset(&tio,0,sizeof(tio));
  
  tio.c_iflag &= ~(ICRNL |    /* disable CR-to-NL mapping */
		   INLCR |    /* disable NL-to-CR mapping */
		   IGNCR |    /* disable ignore CR */
		   ISTRIP |   /* disable stripping of eighth bit */
		   IXON |     /* disable output flow control */
		   BRKINT |   /* disable generate SIGINT on brk */
		   IGNPAR |
		   PARMRK |
		   IGNBRK |
		   INPCK);    /* disable input parity detection */

  tio.c_lflag &= ~(ICANON |   /* enable non-canonical mode */
		   ECHO |     /* disable character echo */
		   ECHOE |    /* disable visual erase */
		   ECHOK |    /* disable echo newline after kill */
		   ECHOKE |   /* disable visual kill with bs-sp-bs */
		   ECHONL |   /* disable echo nl when echo off */
		   ISIG |     /* disable tty-generated signals */
		   IEXTEN);   /* disable extended input processing */
  
  tio.c_cflag |= CS8;         /* enable eight bit chars */
  tio.c_cflag &= ~PARENB;     /* disable input parity check */

  tio.c_oflag &= ~OPOST;      /* disable output processing */

  tio.c_cflag |= CLOCAL;


  tty_fd=open("/dev/ttyUSB0", O_RDWR | O_NONBLOCK);      
  cfsetospeed(&tio,B115200);            // 115200 baud
  cfsetispeed(&tio,B115200);            // 115200 baud
 
  if (tcsetattr(tty_fd, TCSAFLUSH // TCSANOW
		, &tio) < 0)
    {
      cout << "set attr failed" << endl;
      return -1;
    }

  return 1;
}

float get_distance(float x1, float y1, float x2, float y2)
{
  return sqrt(pow(y2-y1, 2) + pow(x2-x1, 2));
}

float get_angle(float x1, float y1, float x2, float y2)
{
  return atan2(y2-y1, x2-x1);
}

Point local_to_global(double carX, double carY, double car_angle, double cordX, double cordY)
{
  if((int)(cordX*100000) == 0)
    cordX = cordX+0.000001;
  
  if((int)(cordY*100000) == 0)
    cordY = cordY+0.000001;

  double x = carX + (get_distance(0,0,cordX,cordY)*(cos(car_angle+(get_angle(0,0,cordX,cordY)))));
  double y = carY + (get_distance(0,0,cordX,cordY)*(sin(car_angle+(get_angle(0,0,cordX,cordY)))));
  
  Point return_xy;
  return_xy.x = x;
  return_xy.y = y;

  return return_xy;
}


uint16_t get_check_sum(int data[])
{
  uint16_t data_list[10];
  int checksum32 = 0;
  uint16_t checksum = 0;
  for(int i = 0; i < 10; i++)
    {
      data_list[i] = (data[2*i] + (data[2*i+1]<<8));
    }
  
  for(int i = 0; i < 10; i++)
    {
      checksum32 = (checksum32 << 1) + data_list[i];
    }

  checksum = (uint16_t)(checksum32 & 0x7FFF) + (checksum32 >> 15);
  checksum = checksum & 0x7FFF;

  return checksum;

} 

void* lidar_thread(void* env)
{
  for(int i = 0; i < 360; i++)
    {
      lidar_x[i] = 0;
      lidar_y[i] = 0;
    }
  uint8_t startbyte;
  uint8_t index;
  uint16_t speed;
  uint8_t data[4];
  uint8_t check[2];
  uint16_t checksum;
  uint32_t dist0;
  uint32_t dist1;
  uint32_t dist2;
  uint32_t dist3;
  uint8_t temp;
  uint8_t flag0 = 1; 
  uint8_t flag1 = 1; 
  uint8_t flag2 = 1; 
  uint8_t flag3 = 1; 

  int arr[20];

  float angle_rad;
  float c, s;
  double x, y;

  int test = 0;

  while(1)
    {
      
      startbyte = 0;
      read(tty_fd, &startbyte, 1);
      usleep(0.01);
      if(startbyte == 0xFA)
	{
	  read(tty_fd, &index, 1);
	  if(index >= 0xA0 && index <= 0xF9)
	    {
	      index = index - 0xA0;
	    }
	  else
	    {
	      continue;
	    }

	  arr[0] = 0xFA;
	  arr[1] = index + 0xA0;

	  for(int i = 0; i < 2; i++)
	    {
	      read(tty_fd, &data[i], 1);
	      //usleep(1);
	      arr[2+i] = data[i];
	    }
	  
	  speed = data[0] | (data[1] << 8);

	  for(int i = 0; i < 4; i++)
	    {
	      read(tty_fd, &data[i], 1);
	      //usleep(1);
	      arr[4+i] = data[i];
	    }
	  flag0 = (data[1] & 0x80) >> 7;
	  dist0 = data[0] | (data[1] & 0x3F) << 8;

	  for(int i = 0; i < 4; i++)
	    {
	      read(tty_fd, &data[i], 1);
	      //usleep(1);
	      arr[8+i] = data[i];
	    }
	  flag1 = (data[1] & 0x80) >> 7;
	  dist1 = data[0] | (data[1] & 0x3F) << 8;

	  for(int i = 0; i < 4; i++)
	    {
	      read(tty_fd, &data[i], 1);
	      // usleep(1);
	      arr[12+i] = data[i];
	    }
	  flag2 = (data[1] & 0x80) >> 7;
	  dist2 = data[0] | (data[1] & 0x3F) << 8;

	  for(int i = 0; i < 4; i++)
	    {
	      read(tty_fd, &data[i], 1);
	      //usleep(1);
	      arr[16+i] = data[i];
	    }
	  flag3 = (data[1] & 0x80) >> 7;
	  dist3 = data[0] | (data[1] & 0x3F) << 8;
	  
	  for(int i = 0; i < 2; i++)
	    {
	      read(tty_fd, &check[i], 1);	      
	      //usleep(1);
	    } 
	  
	  checksum = check[0] | check[1] << 8;

	  if(checksum == get_check_sum(arr))
	    {
	      //	      printf("times checksum is valid: %d :::: checksum: %d\n", test); 
	      //test++;

	      //printf("Flags: 1: %d --- 2: %d --- 3: %d --- 4: %d \n", flag0, flag1, flag2, flag3);
	      
	      if(point_vector.size() > 716)
		{
		  //mtx.lock();
		  point_vector.erase(point_vector.begin(), point_vector.begin()+360);
		  //mtx.unlock();
		}
	      if(flag0 == 0 && dist0 <= 3000)
		{
		  angle_rad = (index * 4 + 0) * M_PI / 180.0;
		  c = cos(angle_rad);
		  s = sin(angle_rad);

		  x = dist0 * c;
		  y = dist0 * s;
		  
		  Point xy;
		  xy.x=x;
		  xy.y=y;
		  point_vector.push_back(xy);
		}
	      if(flag1 == 0 && dist1 <= 3000)
		{
		  angle_rad = (index * 4 + 1) * M_PI / 180.0;
		  c = cos(angle_rad);
		  s = sin(angle_rad);

		  x = dist1 * c;
		  y = dist1 * s;
		  
		  Point xy;
		  xy.x=x;
		  xy.y=y;
		  point_vector.push_back(xy);
		}
	      if(flag2 == 0 && dist2 <= 3000)
		{
		  angle_rad = (index * 4 + 2) * M_PI / 180.0;
		  c = cos(angle_rad);
		  s = sin(angle_rad);

		  x = dist2 * c;
		  y = dist2 * s;
		  
		  Point xy;
		  xy.x=x;
		  xy.y=y;
		  point_vector.push_back(xy);
		}
	      if(flag3 == 0 && dist3 <= 3000)
		{		  
		  angle_rad = (index * 4 + 3) * M_PI / 180.0;
		  c = cos(angle_rad);
		  s = sin(angle_rad);

		  x = dist3 * c;
		  y = dist3 * s;
		  
		  Point xy;
		  xy.x=x;
		  xy.y=y;
		  point_vector.push_back(xy);
		}
	    }
	}
    }
}


void start_lidar()
{
  pthread_t t;
  int x;
  pthread_create(&t, NULL, &lidar_thread, &x);
}
 
int deinit()
{
  close(tty_fd);
  return 1;
}


/* Nif function definitions */

static ERL_NIF_TERM get_lidar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM lidar_xx [720];
  ERL_NIF_TERM lidar_yy [720];
  Point t, t2g;
  
  int count = 0;
  cout << "BEFORE" << endl;
  while(!point_vector.empty() && count < 720)
    {
      t = point_vector.front();
      
      t2g = local_to_global(0, 0, 0, t.x, t.y);
      
      lidar_xx[count] = enif_make_int(env, (int) t2g.x);
      lidar_yy[count] = enif_make_int(env, (int) t2g.y);

      //mtx.lock();
      point_vector.erase(point_vector.begin());
      //mtx.unlock();
      count++;
    }
  cout << "After - count: " << count <<  endl;
  ERL_NIF_TERM returned_data = enif_make_tuple2(env, 
						enif_make_list_from_array(env, lidar_xx, count), 
						enif_make_list_from_array(env, lidar_yy, count));
  
  return returned_data;  
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  //Initializes the serial port
  int result = init_lidar();
  cout << "Lidar LOADING: " << result << endl;
  
  if (result != -1)
    {
      start_lidar();
      return 0;
    }
  else
    {
      return 1;
    }
}

static ERL_NIF_TERM deinit_lidar(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  
  deinit();
  return enif_make_atom(env, "ok");
  
}


static ErlNifFunc nif_funcs[] =
  {
    {"get_lidar", 0, get_lidar},
    {"deinit_lidar", 0, deinit_lidar}
  };

ERL_NIF_INIT(lidar_nif,nif_funcs,load,NULL,NULL,NULL)
