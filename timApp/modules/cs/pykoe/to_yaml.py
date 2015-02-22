# -*- coding: utf-8 -*-
from _codecs import encode

__author__ = 'vesal'

import yaml
# from yaml import CLoader
import yaml.parser
import yaml.scanner
import re


def correct_yaml(text):
    """
      Inserts missing spaces after : Like  width:20 => width: 20
      Also gives an other way to write multiline attributes, by starting
      the multiline like: program: |
      and ending it by | in first column

    :param text: text to convert proper yaml
    :return: text that is proper yaml
    """
    lines = text.splitlines()
    s = ""
    p = re.compile("^[^ :]*:[^ ]")  # kissa:istuu
    pm = re.compile("^[^ :]*:[ ]+\|[^ a-zA-Z]+$")  # program: ||| or  program: |!!!
    multiline = False
    for line in lines:
        if p.match(line) and not multiline:
            line = line.replace(':', ': ', 1)
        if pm.match(line):
            multiline = True
            n = 0
            line, end_str = line.split("|", 1)
            s = s + line + "|\n"
            continue
        if multiline:
            if line == end_str:
                multiline = False
                continue
            line = " " + line
        s = s + line + "\n"
    return s


def parse_yaml(text):
    values = {}

    if len(text) == 0: return False
    try:
        text = correct_yaml(text)
        values = yaml.load(text)
    except yaml.parser.ParserError as e:
        return {'error': "YAML is malformed: " + str(e)}
    except yaml.scanner.ScannerError as e:
        return {'error': "YAML is malformed: " + str(e)}
    try:
        if type(values) is str:
            return {'error': "YAML is malformed: " + values}
        else:
            return {"markup": values}
    except KeyError:
        return {'error': "Missing identifier"}
    return false


if __name__ == '__main__':
    st = ("""
stem:"Kirjoita  5+3 ja paina Aja" # normaali rivi
placeholder: Kirjoita tähän
 jokin:lasku, esim. 5 + 3
p2:"Jotakin ja
jokin:kukku" # tämä menee väärin ja lisää tähän : jälkeen välilyönnin
p3:>
 Jotakin ja
 jokin:kukku
type:cs
maxrows:20
-program:|-
using System;
public class Lausekkeet
{
       LAUSEKE
}
-
replace:LAUSEKE
byCode:|-
eka
-
"""
          )
    markup = parse_yaml(st)
    print(markup)