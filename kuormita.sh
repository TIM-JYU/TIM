#! /bin/bash
. ./variables.sh

if [ $# -ne "1" ]; then
 echo Stress test for TIM instance. Makes n simultaneous wget calls.
 echo Usage examples:
 echo ./kuormita.sh 1
 echo ./kuormita.sh 128
 exit
fi

rm -f  /tmp/120/*
time (  
eval "for i in {1..$1};do wget --timeout=0 localhost:${TIM_PORT}/view/1 -O /tmp/120/120.\$i & done" 2>/dev/null
wait
)

echo "Errors:  `ls -la /tmp/120 | grep -c "  0"` "
