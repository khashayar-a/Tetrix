#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

#include "iostream"
#include <time.h>

using namespace std;


int tty_fd;


int deinit()
{
 
  close(tty_fd);
 
  return 1;
} 
int init()
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

  /* roland */
  tio.c_cflag |= CLOCAL;


  tty_fd=open("/dev/serial/by-id/usb-2476_1050-if00", O_RDWR | O_NOCTTY | O_NDELAY);      
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

void endian_swap_16(uint16_t * x)
{
  *x = (*x >> 8) | 
    (*x << 8); 
} 

void endian_swap_32(uint32_t * x)
{
  *x = (*x >> 24) | 
    ((*x << 8) & 0x00FF0000) |
    ((*x >> 8) & 0x0000FF00) |
    (*x << 24);
}


float heading()
{
  char value[128][128];
  char euler[128][128];
  char temp = 'a';
  float d = 0.0;
  float x,y,z;

  write(tty_fd, ":37\n", 4);
  usleep(10*1000);

  for(int i = 0;;i++)
    { 
      for(int j = 0;; j++)
	{
	  read(tty_fd, &temp, 1);
	  if(temp == ',' || temp == 10){
	    break;
	  }else{	
	    value[i][j] = temp;
	  }
	}
      if(temp == 10)
	{
	  break;
	}
    }
  
  x = atof(value[6]);
  z = atof(value[7]);
  y = atof(value[8]);
  
  if(y>0)
    {
      d = 90 - atan(x/y) * 180 / M_PI;
    }
  if(y<0)
    {
      d = 270 - atan(x/y) * 180 / M_PI;
    } 
  if(y==0)
    { 
      if(x<0)
	{
	  d = 180.0;
	}
      else
	{
	  d = 0.0;
	}
    }
  return d;
}
