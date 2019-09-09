#!/usr/bin/env bash

read -r -d '' SCRIPT <<'EOF'
plugins=(
cs/js
drag/js
#dropdown/js
#feedback/js
#jsrunner/public/javascripts
#multisave/js
#numericfield/js
#pali/js
#svn/js
#textfield/js
)

FAIL=0

for index in ${!plugins[*]}
do
    tslint --exclude '../static/scripts/tim/**/*.ts' --project ${plugins[${index}]} &
    pids[${index}]=$!
done

for pid in ${pids[*]}; do
    wait ${pid} || let "FAIL+=1"
done

if [ "$FAIL" == "0" ];
then
exit 0
else
exit 1
fi
EOF

../../run_command_workdir.sh timApp/modules bash -c "${SCRIPT}"
