wget http://math.tut.fi/mathcheck/mathcheck.zip -O mathcheck.zip -nv
unzip -o mathcheck.zip
g++ -ansi -W -Wall -pedantic -O3 -Dsubhtml mathcheck.cc -o mathcheck_subhtml.out
chmod 755 mathcheck_subhtml.out
rm mathcheck.zip
