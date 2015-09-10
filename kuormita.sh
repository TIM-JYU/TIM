#! /bin/bash
# kutsu kuormita.sh  6  tai vastaavasti
#! /bin/bash
time (
#eval "for i in {1..$1};do wget https://tim.jyu.fi/view/120 -O /tmp/120.$i & done"
eval "for i in {1..$1};do wget localhost:5000/view/ohj1 -O /tmp/120.$i & done"
wait
)

