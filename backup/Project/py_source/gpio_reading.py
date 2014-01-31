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

from erlport import Port, Protocol


fd = 1
mm = 1 
pinData = [0,0,0]


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

def valid_readings(pinsNew):
  if pinsNew[1:] == pinsNew[:-1]:
    return False
  else:
    return True

def setup_pins(self):
  """ Maps GPIO pins to local address and prepares them for reading """

  global fd,mm

  # mmap the GPIO pins into local address space
  # this uses the python mmap interface instead of the C library
  # yeilds around 75khz
  setup_gpio_pin(gpio_sysfs['pin31'], "in")  # this pin powers the level translator low side
  setup_gpio_pin(gpio_sysfs['pin29'], "in")  # this pin gets toggled
  setup_gpio_pin(gpio_sysfs['pin27'], "in")  # this pin gets toggled
  # note: i switch to using the Bunch class to have less typing for gpio_sysfs list

  # Setup gpio pins and mapping
  #gpio_sysfs_setvalue(gsys.pin31,1)     # power the level translator
  #gpio_sysfs_setvalue(gsys.pin29,1)     # start the toggle pin high
  #gpio_sysfs_setvalue(gsys.pin27,1)     # start the toggle pin high
  fd = setup_fd()
  mm = setup_mmap(fd)

def handle_readPins(self):
  """ Reads GPIO pins 27, 29, and 31 """

  global pinData

  pinData[0]= read_gpio(mm, gpio_addresses['pin27'])
  pinData[1]= read_gpio(mm, gpio_addresses['pin31'])
  pinData[2]= read_gpio(mm, gpio_addresses['pin29'])
 
  return (pinData[0], pinData[1], pinData[2])


# ------------------ Class that Erlang communicates with ----------------

"""
class GPIO_Protocol():

  def __init__(self):

    # Setup pins
    self.setup_pins()

    # Stores sensor data
    self.pinData = [0,0,0]

  def __del__(self):
    
    cleanup(self.mm)                 # cleanup python mmap

  def setup_pins(self):

    # mmap the GPIO pins into local address space
    # this uses the python mmap interface instead of the C library
    # yeilds around 75khz
    setup_gpio_pin(gpio_sysfs['pin31'], "in")  # this pin powers the level translator low side
    setup_gpio_pin(gpio_sysfs['pin29'], "in")  # this pin gets toggled
    setup_gpio_pin(gpio_sysfs['pin27'], "in")  # this pin gets toggled
    # note: i switch to using the Bunch class to have less typing for gpio_sysfs list

    # Setup gpio pins and mapping
    #gpio_sysfs_setvalue(gsys.pin31,1)     # power the level translator
    #gpio_sysfs_setvalue(gsys.pin29,1)     # start the toggle pin high
    #gpio_sysfs_setvalue(gsys.pin27,1)     # start the toggle pin high
    self.fd = setup_fd()
    self.mm = setup_mmap(self.fd)

  def handle_readPins(self):

    self.pinData[0]= read_gpio(self.mm, gpio_addresses['pin27'])
    self.pinData[1]= read_gpio(self.mm, gpio_addresses['pin31'])
    self.pinData[2]= read_gpio(self.mm, gpio_addresses['pin29'])
 
    return (self.pinData[0], self.pinData[1], self.pinData[2])


if __name__ == "__main__":
    proto = GPIO_Protocol()
    proto.run(Port(use_stdio=True))
"""
