SHELL := /bin/bash
 #/********************************************************
 ## * Author:    Zlatan Habul
 ## * Created:   28.10.2013
 ## * Makefile
 ## *
 ## ********************************************************/

all: carControl

CC       ?= gcc
CXX      ?= g++

LBITS := $(shell getconf LONG_BIT)
ifeq ($(LBITS),64)
   CXXFLAGS ?= -Wall -g -lpthread
   MAKENIF ?= gcc -fPIC -shared -lstdc++ -o ../ebin/hidnif.so hidnif.cpp ../ebin/carControl.o ../ebin/hid.o -I /usr/lib64/erlang/usr/include/ $(INCLUDES) $(LIBS)
else
   CXXFLAGS   ?= -Wall -g
   MAKENIF ?= g++ -o ../ebin/hidnif.so -fPIC -shared hidnif.cpp -lstdc++ ../ebin/carControl.o ../ebin/hid.o $(LIBS)
endif


COBJS     = hid.o
CPPOBJS   = carControl.o
OBJS      = $(COBJS) $(CPPOBJS)
LIBS      = `pkg-config libusb-1.0 libudev --libs`
INCLUDES ?= -I. `pkg-config libusb-1.0 --cflags `


carControl: $(OBJS)
	$(MAKENIF)
	gcc -fPIC -shared -o ../ebin/gpio_nif.so gpio_nif.c gpio_functions.c -I../c_include/ -pthread 	
	g++ -fPIC -shared -o ../ebin/imgproc_nif.so imgproc_nif.cpp camera_functions.cpp -I../c_include/ -lopencv_core -lopencv_imgproc -lopencv_highgui -lueye_api
	g++ -fPIC -shared -o ../ebin/hal_nif.so hal_nif.cpp hal_functions.cpp -I../c_include/ 
	g++ -fPIC -shared -o ../ebin/three_space_nif.so three_space_nif.cpp three_space_functions.cpp -I../c_include
	g++ -std=c++0x -fPIC -shared -o ../ebin/lidar_nif.so lidar_nif.cpp -pthread

$(COBJS): %.o: %.c
	$(CC) $(CXXFLAGS) -c $(INCLUDES) $< -o ../ebin/hid.o -fPIC 

$(CPPOBJS): %.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $(INCLUDES) $< -o ../ebin/carControl.o -fPIC 

clean:
	shopt -s extglob
	rm -f ../ebin/*.o ../ebin/*.so ../ebin/*.beam
	cp -r ../erlport/ebin/ ../
	
.PHONY: clean

nif:
	$(MAKENIF)

img_proc: 
	g++ -fPIC -shared -o ../ebin/imgproc_nif.so imgproc_nif.cpp camera_functions.cpp -I../c_include/ -lopencv_core -lopencv_imgproc -lopencv_highgui -lueye_api

gpio:
	gcc gpio_functions.c -o gpio_functions -I ../c_include -pthread
