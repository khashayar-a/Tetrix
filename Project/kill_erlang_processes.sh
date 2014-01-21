#!/bin/sh

for pid in `ps aux | grep "[e]rlang" | awk '{print $2}'`; do 
  sudo kill -9 $pid;
done


for pid in `ps aux | grep "[i]nit monitor" | awk '{print $2}'`; do 
  sudo kill -9 $pid;
done
