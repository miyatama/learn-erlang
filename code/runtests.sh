#!/bin/bash
filename="results"
rm ./${filename}
for i in `seq 1 32` ;
do
  echo ${i}
  erl -boot start_clean -noshell -smp +S $i -s ptests tests $i >> ./${filename}
done
