host="localhost"
if [ -n "$2" ]; then host="$2"; echo "$host"; fi
docid="3"
respheader==`curl -s -I "$host"`
#echo "$respheader"
xsrf=`echo "$respheader" |  grep XSRF-TOKEN | sed -r 's/.*XSRF-TOKEN=([^;]*);.*/X-XSRF-TOKEN: \1/'`
session=`echo "$respheader" |  grep "session=" | sed -r 's/.*session=([^;]*);.*/session=\1; lang=fi/'`
ctype="Content-Type: application/json"
cmd='curl -s -b "$session" -H "$xsrf" -H "$ctype" -X PUT "$host/csPlugin/$docid.helloekaprossu/answer/" -d "{\"input\":{}}" |  sed -r "s/.*Aika = ([^\\]*)\\n.*/Prossu: \1\n/g"'
#cmd='echo "kissa"'

dir=/tmp/stresstest/1
mkdir -p ${dir}
rm ${dir}/* 2>/dev/null

time (
eval "for i in {1..$1};do $cmd  & done"
wait
)
echo "Errors: $(ls -la ${dir} | grep -c "  0")"
echo ""