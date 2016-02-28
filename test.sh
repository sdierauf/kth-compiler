#!/bin/bash

echo "Starting tests for:" $@
echo "run --tokens src/$@.slacc" > commands.txt
sbt < commands.txt
echo "run --ast src/$@.slacc" > commands.txt
sbt < commands.txt
