import os
import shutil
print("hello")
print("directory = " + os.curdir)

from os import listdir
from os.path import isfile, join
onlydirs = [ f for f in listdir("/tmp") if not isfile(join("/tmp",f)) ]

for dirname in onlydirs:
    print(dirname)
    if len(dirname) > 33: shutil.rmtree("/tmp/"+dirname)

print(onlydirs)
