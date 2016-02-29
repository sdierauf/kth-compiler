#!/bin/bash

echo "Starting tests for:" $@
echo "run --tap src/$@.slacc" > commands.txt
sbt < commands.txt
