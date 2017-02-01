#!/bin/bash

findcmd1="find . -not \\( -path ./modules/cs/simcir/check -prune \\) -name \*.py"
findexec1="-exec echo 'Formatting {}...' \;"

findexec2="-exec docformatter --wrap-summaries 120 --wrap-descriptions 120 --in-place '{}' \;"

# PyCharm and autopep8 disagree on E251 (Remove whitespace around parameter '=' sign), so we must disable it.
findexec3="-exec autopep8 --ignore E251 --max-line-length 120 --in-place '{}' \;"

./run_command.sh /bin/bash -c \
 "$findcmd1 $findexec1 $findexec2 $findexec3"
