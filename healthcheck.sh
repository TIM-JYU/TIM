#!/usr/bin/env bash

if [ $# -ne 3 ]; then
  echo "Sends error mail if TIM is down."
  echo "Usage:"
  echo "./healthcheck.sh <TIM root URL> <source email> <destination email>"
  exit 1
fi

baseurl="$1"
url="${baseurl}/ping"
interval=1
while true;
do
  status=$(curl -s -o /dev/null -w "%{http_code}" ${url})
  if [ "${status}" != "200" ]
  then
    msg=$(cat << EOF
TIM (${baseurl}) is down. Pinged URL ${url} and got status code ${status} but expected 200.

Checking again in ${interval} minutes.

EOF
    )
    echo -e "${msg}"
    echo -e "${msg}" | mailx -r "$2" -s "TIM (${baseurl}) is down" "$3"
    sleep $(( $interval * 60 ))
    interval=$(( ${interval} + 1 ))
  else
    interval=1
    echo "Ping successful. Checking again in 1 minute."
    sleep $(( $interval * 60 ))
  fi
done
