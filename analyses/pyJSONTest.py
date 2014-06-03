#!/usr/bin/python
import json
import sys
import warnings

#warnings.filterwarnings('error')

def main():
    try:
        input = sys.stdin.read()
        dump  = json.dumps(input)
        read  = json.loads(dump)
        try:
            if read != input: 
                print "--input--"
                print input
                print "--output--"
                print read
                return 255
        except UnicodeException: 
                print "--input--"
                print input
                print "--output--"
                print read
                return 254
    except Exception: return 1
    return 0

main()
