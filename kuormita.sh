#! /bin/bash
# kutsu kuormita.sh  6  tai vastaavasti
#! /bin/bash
rm -f  /tmp/120/*
time (  
eval "for i in {1..$1};do wget --timeout=0 localhost:50001/view/1 -O /tmp/120/120.\$i & done" 2>/dev/null
#eval "for i in {1..$1};do wget localhost:5000/view/ohj1 -O /tmp/120.$i & done"
wait
)

echo "Vikoja  `ls -la /tmp/120 | grep -c "  0"` "

