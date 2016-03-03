#!/bin/bash

echo "Lexing:" $@
echo "run --tokens src/test/lab2/$@.slac" | sbt

