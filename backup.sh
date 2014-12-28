#!/bin/bash
echo Backing up files
XZ_OPT=-9 tar --xz --checkpoint=.1000 -cf ~/${PWD##*/}.backup.tar.xz .
echo -e "\nDone. Backup saved to home directory."
