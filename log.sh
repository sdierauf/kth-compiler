#!/bin/bash

echo "Starting tests for:" $@
echo "run --tap src/$@.slacc" | sbt

