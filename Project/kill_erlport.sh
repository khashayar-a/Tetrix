#!/bin/sh

for pid in `ps aux | grep "[e]rlport" | awk '{print $2}'`; do 
  kill -9 $pid;
done
