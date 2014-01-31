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


  tty_fd=open("/dev/serial/by-id/usb-2476_1010-if00", O_RDWR | O_NOCTTY | O_NDELAY);      
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



void set_compass(int flag)
{
  if (flag){
    write(tty_fd, ":109,1\n", 7);
    usleep(10*1000);
  }
  else{
    write(tty_fd, ":109,0\n", 7);
    usleep(10*1000);
  }
}



float heading()
{
  char value[128][128];
  char euler[128][128];
  char temp = 'a';
  float x, y, z, w; // quaternion
  float yaw, pitch, roll; // euler angles
  float heading, attitude, bank;
  
  
  write(tty_fd, ":0\n", 3);
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

  x = atof(value[0]);
  y = atof(value[1]);
  z = atof(value[2]);
  w = atof(value[3]);

  double test = x*y + z*w;
  if (test > 0.499) { // singularity at north pole
    heading = 2 * atan2(x,w);
    attitude = M_PI/2;
    bank = 0;
  }
  if (test < -0.499) { // singularity at south pole
    heading = -2 * atan2(x,w);
    attitude = - M_PI/2;
    bank = 0;
  }else{
    double sqx = x*x;
    double sqy = y*y;
    double sqz = z*z;
    heading = atan2(2*y*w - 2*x*z , 1 - 2*sqy - 2*sqz);
    attitude = asin(2*test);
    bank = atan2(2*x*w - 2*y*z , 1 - 2*sqx - 2*sqz);
  }
    


  //  printf("Heading: %f \n", heading);

  return heading;

}
