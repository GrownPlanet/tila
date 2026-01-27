#!/bin/bash

mkdir out 2> /dev/null
dune exec tilang test.tl out/out.s
cd out
./spasm out.s out.8xp
