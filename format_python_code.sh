#!/usr/bin/env bash

# To format all Python files, run without parameters.
# To format specific file(s), use: ./format_python_code.sh filename

name='\*.py'
if [ $# -eq 1 ]; then
 name="\*$1\*.py"
fi

findcmd1="find . -not \\( -path ./modules/cs/simcir/check -prune \\) -name $name"
findexec1="-exec echo 'Formatting {}...' \;"

findexec2="-exec docformatter --wrap-summaries 120 --wrap-descriptions 120 --in-place '{}' \;"

# PyCharm and autopep8 disagree on E251 (Remove whitespace around parameter '=' sign), so we must disable it.
findexec3="-exec autopep8 --ignore E251 --max-line-length 120 --in-place '{}' \;"

./r /bin/bash -c \
 "$findcmd1 $findexec1 $findexec2 $findexec3"
