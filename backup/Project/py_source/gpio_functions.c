#include "gpio_functions.h"

int pins[3] = {0, 0, 0};
int pinsNew[3] = {0, 0, 0};
int movement = 0;

void *map_base, *virt_addr;
int fd;


int main()
{

  map_pins();

  setup_gpiopin(PIN31Channel, PIN31Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN29Channel, PIN29Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN27Channel, PIN27Bit, PULLDS, INPUT); 

  pthread_t loop; 
  pthread_create(&loop, NULL, &loop_retrieving,NULL);
    
  int *results;
  int resultSaved[3] = {0, 0, 0};
  while(1){
      results = get_gpio_data();

      if(results[0] != resultSaved[0] || results[1] != resultSaved[1] ||results[2]!=resultSaved[2]){
        printf("( %d, %d, %d )\n", results[0], results[1], results[2]);
        resultSaved[0] = results[0];
        resultSaved[1] = results[1];
        resultSaved[2] = results[2];
//        printf("movement: %d", get_movement());
      }
  }
  
  return 0;
}


void initialize_pin_reading()
{

  map_pins();

  setup_gpiopin(PIN31Channel, PIN31Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN29Channel, PIN29Bit, PULLDS, INPUT); 
  setup_gpiopin(PIN27Channel, PIN27Bit, PULLDS, INPUT); 

  pthread_t loop; 
  pthread_create(&loop, NULL, &loop_retrieving,NULL);

}
int map_pins()
{
  off_t target = EXYNOS;
  if((fd = open("/dev/mem", O_RDWR | O_SYNC)) == -1) {

      printf("Error opening /dev/mem, can't setup file descriptors for GPIO\n");
      return 1;
  } 

  map_base = mmap(0, MAP_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, target & ~MAP_MASK);
  if(map_base == (void *) -1) {
        
        printf("Mapping of GPIO space failed!!\n");
        return 1;
  }   

}

void setup_gpiopin(int channel, int bit, int value, int pullval) {
	div_t div_res;
	unsigned char val, tmp, hld;
	unsigned char * base;

	base = (map_base + channel) - GPIO_GPCONREG;
	div_res = div (bit, 2);		// 2 nibbles per byte so divide by 2
	base += div_res.quot;
	val  = *(unsigned char *) base;
	if (value) {				// non-zero means set 0001=output
		if (div_res.rem) {		// if remainder then its upper nibble
			val &= 0b00011111;	// upper nibble, not always def to zero
			val |= 0b00010000;	// set upper nibble as output
		} else {				// otherwise its lower nibble
			val &= 0b11110001;	// not always def to zero on boot
			val |= 0b00000001;	// set lower nibble as output
		}
	} else {					// otherwise set 0000=input
		if (div_res.rem) {		// if remainder then its upper nibble
			val &= 0b00001111;	// clear upper nibble to be input
		} else {				// otherwise its lower nibble
			val &= 0b11110000;	// clear lower nibble to be input
		}
	}				
	*(unsigned char *) base = val;	
	// base = *(unsigned char *)val;	
	base = (map_base + channel) + GPIO_UPDOWN;
	if      (pullval == PULLUP) {
		tmp = 0b00000010;		// pullup enabled
	}
	else if (pullval == PULLDN) {
		tmp = 0b00000001;		// pulldown enabled
	} 
	else {
		tmp = 0;				// disable pullup/down
	}
	if (bit < 4) {
		hld = tmp << (bit*2);	// shift the 2 bits to their proper location
	} else {
		bit = bit - 4;
		hld = tmp << (bit*2);	// shift the 2 bits to their proper location
		base++;					// move up to next byte
	}
	val = *(unsigned char *) base;

	val |= hld;
}


int read_gpio_pin(int channel, int bit )
{ 
  int input;
  input = *(unsigned char *) (map_base + channel);
	if (input & (1 << bit)) {
    return 1;	
	} else {
    return 0;
	}

}

//void loop_retrieving()
void* loop_retrieving(void *arg)
{

  int values_changed = 0;

  while(1)
  {
    
    pinsNew[0] = read_gpio_pin(PIN27Channel, PIN27Bit);
    pinsNew[1] = read_gpio_pin(PIN31Channel, PIN31Bit);
    pinsNew[2] = read_gpio_pin(PIN29Channel, PIN29Bit);

    if(is_movement())
    {
      movement += calculate_movement(pins_state(pins), pins_state(pinsNew));
      pins[0] = pinsNew[0];
      pins[1] = pinsNew[1];
      pins[2] = pinsNew[2];
    }

    usleep(100);

  }
 
}

int is_movement()
{
    if(pins[0] != pinsNew[0])
      return 1;
    else if(pins[1] != pinsNew[1])
      return 1;
    else if(pins[2] != pinsNew[2])
      return 1;
    else
      return 0;
}

int *get_gpio_data()
{ 
  return pins;
}

int pins_state(int pin_data[])
{
  if (pin_data[0] == 1 && pin_data[1] == 0 && pin_data[2] == 0)  
    return 1; 
  else if (pin_data[0] == 1 && pin_data[1] == 1 && pin_data[2] == 0) 
    return 2; 
  else if (pin_data[0] == 0 && pin_data[1] == 1 && pin_data[2] == 0)  
    return 3; 
  else if (pin_data[0] == 0 && pin_data[1] == 1 && pin_data[2] == 1)  
    return 4; 
  else if (pin_data[0] == 0 && pin_data[1] == 0 && pin_data[2] == 1)  
    return 5; 
  else if (pin_data[0] == 1 && pin_data[1] == 0 && pin_data[2] == 1)  
    return 6; 

  return 0;

}

int calculate_movement(int before, int after)
{

 if (before == after)
    return 0;
 else if (after > before)
    return after - before;
 else     
    return after - before + 6;
}

int get_movement()
{
  int return_value = movement;
  movement = 0;
  return return_value;
  

}
