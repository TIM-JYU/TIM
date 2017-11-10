#!/usr/bin/env bash

if [ $# -ne "2" ]; then
 echo Stress test for TIM instance. Makes n simultaneous wget calls.
 echo "Usage: ./stresstest.sh <number of calls> <doc id or path>"
 echo Examples:
 echo ./stresstest.sh 1 1
 echo ./stresstest.sh 128 1
 exit
fi

read -r -d '' SCRIPT <<'EOF'
dir=/tmp/stresstest/$2
mkdir -p ${dir}
rm ${dir}/* 2>/dev/null
time (
eval "for i in {1..$1};do wget --timeout=0 tim:5000/view/$2 -O ${dir}/stresstest.\$i & done" 2>/dev/null
wait
)
echo "Errors: $(ls -la ${dir} | grep -c "  0")"
EOF

./docker-compose.sh exec csplugin /bin/bash -c "${SCRIPT}" "" "$@"
