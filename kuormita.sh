#! /bin/bash
# kutsu kuormita.sh  6  tai vastaavasti
#! /bin/bash
time (  
eval "for i in {1..$1};do wget localhost:50001/view/120 -O /tmp/120/120.\$i & done" 2>/dev/null
#eval "for i in {1..$1};do wget localhost:5000/view/ohj1 -O /tmp/120.$i & done"
wait
)

