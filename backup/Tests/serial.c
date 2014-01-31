#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

#include "iostream"
#include <time.h>

using namespace std;


int tty_fd;


 
int init()
{
  struct termios tio;


  if (tcgetattr(tty_fd,&tio) < 0) 
    {
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
                        ISIG |            /* disable tty-generated signals */
                        IEXTEN);   /* disable extended input processing */
  
  tio.c_cflag |= CS8;         /* enable eight bit chars */
  tio.c_cflag &= ~PARENB;     /* disable input parity check */

  tio.c_oflag &= ~OPOST;      /* disable output processing */

  /* roland */
  tio.c_cflag |= CLOCAL;


  tty_fd=open("/dev/ttyUSB0", O_RDWR);      
  cfsetospeed(&tio,B9600);            // 115200 baud
  cfsetispeed(&tio,B9600);            // 115200 baud
 
  if (tcsetattr(tty_fd, TCSANOW, &tio) < 0)
    {
	return -1;
    }

  return 1;
}
 

unsigned char read_char() 
{
  unsigned char c = 'Y';
  //  int count = 0 ;
  // while(c == 'Y'){
  // count ++;

    read(tty_fd,&c,1);
    //}
    //cout << count << endl;
  return c;
  
}

int deinit()
{
 
  close(tty_fd);
 
  return EXIT_SUCCESS;
}


int main()
{

  cout << "heallo" << endl;
  
  cout << init() << endl;

  int count = 0;
  unsigned char c; 
  while(count < 1000000){
    count ++;
    if ((c = read_char()) != 'Y') {
      cout << c  << " -- " << count << endl;
    }
  }
/*  cout << read_char() << endl;
   
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    cout << read_char() << endl;
    }*/
  
  cout << deinit() << endl;


  return 0;

}
