#!/usr/bin/python
# Markham Thomas   April 1, 2013
# Version 1.1      April 9, 2013   modified C library for non-SYSF GPIO setup
# Version 1.2      OCT  15, 2013   added support for OdroidXU
#
# OdroidX, X2 and XU python module allowing memory mapped GPIO access
#
# This supports both pure python mmap of gpio and a C library that does mmap also
# for performance you can call the C version
import os, mmap, sys, time
from ctypes import *
from hkgpiolib import *
from copy import deepcopy
from time import sleep

import datetime as dt
import threading

pins = [0,0,0]
movement = 0
run = True

#----------- set the hardkernel board type here
HKBOARD = 'X2'
#HKBOARD = 'X2' # also works for odroidx

class Bunch(dict):
        def __init__(self, d = {}):
                dict.__init__(self, d)
                self.__dict__.update(d)
        def __setattr__(self, name, value):
                dict.__setitem__(self, name, value)
                object.__setattr__(self, name, value)
        def __setitem__(self, name, value):
                dict.__setitem__(self, name, value)
                object.__setattr__(self, name, value)
        def copy(self):
                return Bunch(dict.copy(self))

MAP_MASK = mmap.PAGESIZE - 1
if HKBOARD == 'X2':
	gpio_addr =  0x11400000   # odroidx and odroidx2 base GPIO address
else:
	gpio_addr = 0x13400000    # odroidXU base GPIO address
	OUTPUT = 1
	INPUT  = 0
	PULLDS = 0  # disable pullup/down
	PULLUP = 1  # enable pullup
	PULLDN = 2  # enable pull down
	# When you do not use or connect port to an input pin without Pull-up/Pull-down then do not leave a port in Input Pull-up/Pull-down disable state. It may cause unexpected state and leakage current. Disable Pull-up/Pull-down when you use port as output function.
	# ----
	# GPF0DAT = 0x0184  - 4  = GPF0CON (0x0180)  byte0 = low nibble is either 0000=input or 0001=output hi-nibble same for next port
	# this repeats for 4 GPIO registers up to 0x0183
	# ----
	# GPF0UPD = 0x0188 (4 registers controlled by 2 bits each: 00=pull-up/down disabled, 01=pull-down enabled, 10=pull-up enabled
	# ----
	# GPF0DRV = 0x018c (drive strength control register (2-bits each) 00=1x, 10=2x, 01=3x, 11=4x
	# ----
	# notes:  when using sysfs interface to set GPIO pin state you must delay 800 clocks before expecting the
	# C library to be able to read a GPIO register, set one bit, and write it back out.  If you immediately
	# read the value after setting one bit in the same register chip, the next bit set to the same GPIO chip
	# will not see a previous bit if it was in the same GPIO chip without waiting those 800 clocks.
	# ----

if HKBOARD == 'X2':
	# maps the GPIO pin to the sysfs /sys/class/gpio/gpioXXX
	gpio_sysfs = { 'pin17':112, 'pin18':115, 'pin19':93, 'pin20':100, 'pin21':108, 'pin22':91, 'pin23':90,
		       'pin24':99,  'pin25':111, 'pin26':103,'pin27':88,  'pin28':98,  'pin29':89, 'pin30':114,
		       'pin31':87,  'pin33':94,  'pin34':105,'pin35':97,  'pin36':102, 'pin37':107,'pin38':110,
		       'pin39':101, 'pin40':117, 'pin41':92, 'pin42':96,  'pin43':116, 'pin44':106,'pin45':109,}
# maps the GPIO pin to the memory mapped offset and bit
	gpio_addresses = { 'pin17':[0x01c4,7], 'pin18':[0x01e4,1], 'pin19':[0x0184,6], 'pin20':[0x01a4,4],
			   'pin21':[0x01c4,3], 'pin22':[0x0184,4], 'pin23':[0x0184,3], 'pin24':[0x01a4,3],
			   'pin25':[0x01c4,6], 'pin26':[0x01a4,7], 'pin27':[0x0184,1], 'pin28':[0x01a4,2],
			   'pin29':[0x0184,2], 'pin30':[0x01e4,0], 'pin31':[0x0184,0], 'pin33':[0x0184,7],
			   'pin34':[0x01c4,0], 'pin35':[0x01a4,1], 'pin36':[0x01a4,6], 'pin37':[0x01c4,2],
			   'pin38':[0x01c4,5], 'pin39':[0x01a4,5], 'pin40':[0x01e4,3], 'pin41':[0x0184,5],
			   'pin42':[0x01a4,0], 'pin43':[0x01e4,2], 'pin44':[0x01c4,1], 'pin45':[0x01c4,4],}
else: # must be odroidxu board
# maps the GPIO pin to the sysfs /sys/class/gpio/gpioXXX
	gpio_sysfs = { 'pin13':309, 'pin14':316, 'pin15':306, 'pin16':304, 'pin17':310, 'pin18':307, 'pin19':319,
		       'pin20':317,  'pin21':318, 'pin22':320,'pin23':315,  'pin24':314,  'pin25':311, 'pin26':313,
		       'pin27':323,}  
# maps the GPIO pin to the memory mapped offset and bit
	gpio_addresses = { 'pin13':[0x0c24,5], 'pin14':[0x0c44,3], 'pin15':[0x0c24,2], 'pin16':[0x0c24,0],
			   'pin17':[0x0c24,6], 'pin18':[0x0c24,3], 'pin19':[0x0c44,6], 'pin20':[0x0c44,4],
			   'pin21':[0x0c44,5], 'pin22':[0x0c44,7], 'pin23':[0x0c44,2], 'pin24':[0x0c44,1],
			   'pin25':[0x0c24,7], 'pin26':[0x0c44,0], 'pin27':[0x0c64,1], }

gpio = Bunch(gpio_addresses)  # GPIO register interface
gsys = Bunch(gpio_sysfs)    # sysfs interface

# pure python mmap setup code
def setup_fd():
  try:
    f = os.open("/dev/mem", os.O_RDWR | os.O_SYNC)
  except:
    print "Open of /dev/mem failed: ", sys.exc_info()[0]
    raise
  return f
def setup_mmap(f):
  try:
    m = mmap.mmap(f, mmap.PAGESIZE, mmap.MAP_SHARED, mmap.PROT_WRITE | mmap.PROT_READ, offset=gpio_addr & ~MAP_MASK)
  except:
    print "mmap of GPIO space failed: ", sys.exc_info()[0]
    raise
  return m
def read_offset(m, offset):
  m.seek(offset)
  a = m.read_byte()
  b = ord(a)
  return b
def read_gpio(m, pin):
  offset = pin[0]
  bit = pin[1]
  m.seek(offset)
  a = m.read_byte()
  b = ord(a)
  c = b >> bit
  c &= 1
  return c
def write_gpio(m, pin, value):
  offset = pin[0]
  bit = pin[1]
  m.seek(offset)
  a = m.read_byte()
  b = ord(a)
  #c = 0
  c = 1 << bit    # shift the 1 to the correct bit number
  if (value):     # if 1 do this
    b |= c      # set the bit in the GPIO byte without destorying the other bits  
  else:
    c ^= 0xff     # xor the bits with ff to flip the 1 to zero and 0's to 1's
    b &= c      # clears the correct bit
  m.seek(offset)
  m.write_byte(str(b))
def cleanup(m):
  m.close()
def setup_gpio_pin(pin, direction):
  a = '/sys/class/gpio/gpio' + str(pin) + '/direction'
  try:
    open(a).read()
  except:
      # it isn't, so export it
      open('/sys/class/gpio/export', 'w').write(str(pin))
  open(a, 'w').write(direction)
def cleanup_gpio_pin(pin):
  open('/sys/class/gpio/unexport', 'w').write(str(pin))
def gpio_sysfs_setvalue(pin, value):
  # when writing a 1 it briefly drops to 0 for 100 usec (no idea why)
  a = '/sys/class/gpio/gpio' + str(pin) + '/value'
  try:
    open(a, 'w').write(str(value))
  except IOError:
    print "Cannot open:", a
    print "  Did you setup that pin?"
    sys.exit(-1)
  except:
    print "GPIO pin not setup (missing): /sys/class/gpio/gpio" + str(pin)
    sys.exit(-1)
def gpio_sysfs_getvalue(pin):
  a = '/sys/class/gpio/gpio' + str(pin) + '/value'
  try:
    b = open(a).read()
  except:
    print "GPIO pin not setup (missing): /sys/class/gpio/gpio" + str(pin)
    sys.exit(-1)
  return b
#-------- example code ---------
def python_mmap_example():
  # mmap the GPIO pins into local address space
  # this uses the python mmap interface instead of the C library
  # yeilds around 75khz
  setup_gpio_pin(gpio_sysfs['pin31'], "out")  # this pin powers the level translator low side
  setup_gpio_pin(gpio_sysfs['pin27'], "out")  # this pin gets toggled
  # note: i switch to using the Bunch class to have less typing for gpio_sysfs list
  gpio_sysfs_setvalue(gsys.pin31,1)     # power the level translator
  gpio_sysfs_setvalue(gsys.pin27,1)     # start the toggle pin high
  fd = setup_fd()
  mm = setup_mmap(fd)
  for i in range(500000):
    write_gpio(mm, gpio_addresses['pin27'], 0)
    print 'wrote 0 ', read_gpio(mm, gpio_addresses['pin27'])
    write_gpio(mm, gpio_addresses['pin27'], 1)
    print 'wrote 1 ', read_gpio(mm, gpio_addresses['pin27'])
  cleanup(mm)                 # cleanup python mmap
def python_sysfs_example():           # pure sysfs interface (no mmap)
  # sysfs GPIO toggle test below yeilds 4.35khz
  setup_gpio_pin(gpio_sysfs['pin31'], "out")  # this pin powers the level translator low side
  setup_gpio_pin(gpio_sysfs['pin27'], "out")  # this pin gets toggled
  gpio_sysfs_setvalue(gsys.pin31,1)     # power the level translator
  gpio_sysfs_setvalue(gsys.pin27,1)     # start the toggle pin high
  for i in range(50000):
    gpio_sysfs_setvalue(gpio_sysfs['pin27'], 1)
    gpio_sysfs_setvalue(gpio_sysfs['pin27'], 0)
def c_mmap_example():             # pure C library access of GPIO registers
  # setup the GPIO pins for measuring with the logic analyzer
  # PULLDS disables pullup or pulldown (use those for input settings)
  setup_gpio()  # setup GPIO mmap in the C library
  setup_gpiopin(gpio.pin31[0],gpio.pin31[1], PULLDS, OUTPUT)  # this pin powers the level translator low side
  setup_gpiopin(gpio.pin27[0],gpio.pin27[1], PULLDS, OUTPUT)  # toggle this pin
  gpio_write(gpio.pin31[0],gpio.pin31[1],1) # initially high
  time.sleep(0.005) # needed since GPIO state change doesn't update the mmap'd GPIO space for some clock cycles
  gpio_write(gpio.pin27[0],gpio.pin27[1],1) # initially high
  # note: 50000000 is about 22 seconds of toggling GPIO
  gpio_toggle(gpio.pin27[0],gpio.pin27[1], 25000000)    # gives 2.4mhz 
  gpio_shutdown()

def valid_readings(pinsNew):
  if pinsNew[1:] == pinsNew[:-1]:
    return False
  else:
    return True

def read_3_pins():


	# mmap the GPIO pins into local address space
	# this uses the python mmap interface instead of the C library
	# yeilds around 75khz
	setup_gpio_pin(gpio_sysfs['pin31'], "in")  # this pin powers the level translator low side
	setup_gpio_pin(gpio_sysfs['pin29'], "in")  # this pin gets toggled
	setup_gpio_pin(gpio_sysfs['pin27'], "in")  # this pin gets toggled
	# note: i switch to using the Bunch class to have less typing for gpio_sysfs list
	
  #setup_gpio()
  #gpio_sysfs_setvalue(gsys.pin31,1)     # power the level translator
  #gpio_sysfs_setvalue(gsys.pin29,1)     # start the toggle pin high
  #gpio_sysfs_setvalue(gsys.pin27,1)     # start the toggle pin high
	fd = setup_fd()
	mm = setup_mmap(fd)


  #write_gpio(mm, gpio_addresses['pin27'], 1)

	n1 = dt.datetime.now()
  #for i in range(3):

	global movement
	global pins
	pinsNew = [0,0,0]
  #pin27 = 0
  #pin29 = 0
  #pin31 = 0


  #pin27a = 0
  #pin29a = 0
  #pin31a = 0
	global run
	while run:
		sleep(0.001)
		pinsNew[0]= read_gpio(mm, gpio_addresses['pin27'])
		pinsNew[1]= read_gpio(mm, gpio_addresses['pin31'])
		pinsNew[2]= read_gpio(mm, gpio_addresses['pin29'])
    #print 'test - pin27: ', pinsNew[0], ', pin29: ', pinsNew[1], ', pin31: ', pinsNew[2] 
	  
		if pins[0] != pinsNew[0] or pins[1] != pinsNew[1] or pins[2] != pinsNew[2]:
      #print 'test - pin27: ', pin27a, ', pin29: ', pin29a, ', pin31: ', pin31a 
	    

      #pin27 = pin27a
      #pin29 = pin29a
      #pin31 = pin31a

      #if valid_readings(pinsNew): 
			if valid_readings(pinsNew):
				movement = movement + calculate_movement(pins_state(pins), pins_state(pinsNew))
				pins = deepcopy(pinsNew)
				print 'MOVEMENT : ', movement
        #pins = pinsNew
        #pins[0] = pinsNew[0]
        #pins[1] = pinsNew[1]
        #pins[2] = pinsNew[2]
#				print 'pin27: ', pins[0], ', pin29: ', pins[1], ', pin31: ', pins[2] , pins_state(pins)

        #print 'changed value!'
			else:
				print '****BAD VALUE!!!****'

    #print 'pin27: ', pin27, ', pin29: ', pin29, ', pin31: ', pin31 
    #print 'read 1 from pin27: ', read_gpio(mm, gpio_addresses['pin27'])
    #print 'read 1 from pin29: ', read_gpio(mm, gpio_addresses['pin29'])
    #print 'read 1 from pin31: ', read_gpio(mm, gpio_addresses['pin31'])

	n2 = dt.datetime.now()
	print 'time to complete: ', (n2-n1).microseconds , ' microseconds'

	cleanup(mm)                 # cleanup python mmap


def c_mmap_odroidxu_test(): # using odroidxu expansion board
	setup_gpio()
	setup_gpiopin(gpio.pin15[0],gpio.pin15[1], PULLDS, INPUT)
	a = gpio_read(gpio.pin15[0],gpio.pin15[1])        # int 10 switch 
	print a
	time.sleep(5)             # sleep 
	a = gpio_read(gpio.pin15[0],gpio.pin15[1])        # int 10 switch 
	print a

# note this should now not require GPIO SYSFS be compiled in for the pure C library calls
# if you uncomment the others and try them they DO use SYSFS
#c_mmap_odroidxu_test()
#c_mmap_example()     # uses the C library to toggle pin27
#python_sysfs_example()   # pure python sysfs interface to toggle bits
#python_mmap_example()   # uses mmap inside of python to improve performance



def calculate_movement(before , after):
	if before == after:
		return 0
	if after > before:
		return after - before
	if after < before:
		return after - before + 6



def pins_state(pins):
	if pins[0] == 1 and pins[1] == 0 and pins[2] == 0 :
		return 1
	if pins[0] == 1 and pins[1] == 1 and pins[2] == 0 :
		return 2
	if pins[0] == 0 and pins[1] == 1 and pins[2] == 0 :
		return 3
	if pins[0] == 0 and pins[1] == 1 and pins[2] == 1 :
		return 4
	if pins[0] == 0 and pins[1] == 0 and pins[2] == 1 :
		return 5
	if pins[0] == 1 and pins[1] == 0 and pins[2] == 1 :
		return 6
	return 0

#def calc_movement(pins, pinsNew):
#	1



def stop_it():
	global run
	run = False
	return 1

def start_it():
	global run
	run = True
	my_thread = threading.Thread(target = read_3_pins)
	my_thread.start()
	return 1


def get_movement():
	global movement
	return_value = movement
	movement = 0
	return return_value

#read_3_pins()
