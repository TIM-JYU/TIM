#!/bin/bash
cd $1
rm -R $3/*
mkdir -p $2
rm csdoc
ln -s $2 csdoc
doxygen /cs/doxygen/Doxyfile