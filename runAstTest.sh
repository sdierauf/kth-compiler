#!/bin/bash

echo "Starting tests for:" $@
echo "run --testAst src/test/$@/valid" | sbt

