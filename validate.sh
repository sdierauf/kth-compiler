#!/bin/bash

echo "Validating src/$@.slacc"
echo "run --ppt src/$@.slacc" | sbt

