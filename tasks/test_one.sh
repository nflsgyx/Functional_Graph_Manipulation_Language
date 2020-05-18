#!/bin/bash
# $1 is the test file name without pfix like test1
# set -x
tname=$(basename $1 .mc)
tpath=tests/$tname.mc
clear
printf "" > testone.log

printf "\n\n-----Verifying the test------\n"  >> testone.log
# ./gmail.native -a $tpath &>> testone.log 
# ./gmail.native -s $tpath &>> testone.log 
# ./gmail.native -l $tpath &>> testone.log 
./gmail.native -c $tpath &>> testone.log 

printf "\n\n-----compiling the test------\n"  >> testone.log
./testall.sh $tpath &>> testone.log



printf "\n\n-------running the test bro-------\n"  >> testone.log
./$tname.exe &>> testone.log
cat testone.log




echo -e "Above is for \e[31m${tname}\e[0m"

./$tname.exe