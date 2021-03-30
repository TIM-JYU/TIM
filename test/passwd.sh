host="https://penetimppa.vrl.jyu.fi"
if [ -n "$1" ]; then host="$1"; echo "$host"; fi

username="akankka"
passwd="kissa"

respheader==`curl -s -I "$host"`
#echo "$respheader"
xsrf=`echo "$respheader" |  grep XSRF-TOKEN | sed -r 's/.*XSRF-TOKEN=([^;]*);.*/X-XSRF-TOKEN: \1/'`
session=`echo "$respheader" |  grep "session=" | sed -r 's/.*session=([^;]*);.*/session=\1; lang=fi/'`
ctype="Content-Type: application/json"
referer="referer: https://penetimppa.vrl.jyu.fi/view/pentest/instructions" 

curl -s -b "$session" -H "$xsrf" -H "$ctype" -H "$referer" -X POST "$host/checkTempPass" -d "{email: \"$username\", token: \"$passwd\"}" 

