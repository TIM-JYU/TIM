# Stop running the script if an error occurs.
set -e
set -o pipefail

if [[ $# -eq 0 ]] ; then
    echo 'Domain name not given. Run this script like: ./script.sh my-tim.example.com'
    exit 1
fi
