#!/usr/bin/env bash
cd $1
rm -R $3/*
mkdir -p $2
rm csdoc
ln -s $2 csdoc
doxygen /cs/doxygen/Doxyfile
# Poista se ettei scrollaa vaan pääikkunaa
sed -i s!if.*updateLocation.*window.location.href=aname.!! csdoc/html/navtree.js # >csdoc/html/navtree.tmp
# sed -i s!...#doc-content...scrollTop.0..!! csdoc/html/navtree.js
# sed -i 's!(var o = new Object())!\1 return!' csdoc/html/navtree.js
#cp csdoc/html/navtree.tmp csdoc/html/navtree.js