host="localhost"
if [ -n "$1" ]; then host="$1"; echo "$host"; fi
docid="3"
respheader==`curl -s -I "$host"`
#echo "$respheader"
xsrf=`echo "$respheader" |  grep XSRF-TOKEN | sed -r 's/.*XSRF-TOKEN=([^;]*);.*/X-XSRF-TOKEN: \1/'`
session=`echo "$respheader" |  grep "session=" | sed -r 's/.*session=([^;]*);.*/session=\1; lang=fi/'`
ctype="Content-Type: application/json"

curl -s -b "$session" -H "$xsrf" -H "$ctype" -X PUT "$host/csPlugin/$docid.helloekaprossu/answer/" -d "{\"input\":{}}" |  sed -r 's/.*Aika = ([^\\]*)\\n.*/Prossu: \1\n/g'
curl -s -b "$session" -H "$xsrf" -H "$ctype" -X PUT "$host/csPlugin/$docid.helloeka/answer/" -d "{\"input\":{}}"     |  sed -r 's/.*runtime":" *([^ ]*) .*/Java:   \1\n/g'
curl -s -b "$session" -H "$xsrf" -H "$ctype" -X PUT "$host/csPlugin/$docid.shell/answer/" -d "{\"input\":{}}"        |  sed -r 's/.*runtime":" *([^ ]*) .*/Shell:  \1\n/g'

