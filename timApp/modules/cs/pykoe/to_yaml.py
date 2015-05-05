# -*- coding: utf-8 -*-
from _codecs import encode

__author__ = 'vesal'
import os
import yaml
# from yaml import CLoader
import yaml.parser
import yaml.scanner
import re


def correct_yaml(text):
    """
      Inserts missing spaces after : Like  width:20 => width: 20
      Also gives an other way to write multiline attributes, by starting
      the multiline like: program: |!!  (!! could be any number and any non a-z,A-Z chars
      and ending it by !! in first column

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
        values = yaml.load(text)  # , Loader=CLoader)
    except yaml.parser.ParserError as e:
        return str(e)
    except yaml.scanner.ScannerError as e:
        return str(e)
    try:
        if type(values) is str:
            return values
        else:
            return values
    except KeyError:
        return "Missing identifier"
    return "Unknown error"


def parse_plugin_values(nodes):
    plugins = []
    for node in nodes:
        values = {}
        name = node['plugin']

        if not len(node.text): continue
        try:
            values = parse_yaml(node.text)
            if type(values) is str:
                plugins.append({"plugin": name, 'error': "YAML is malformed: " + values})
            else:
                plugins.append({"plugin": name, "markup": values, "taskId": node['id']})
        except Exception as e:
            plugins.append({"plugin": name, 'error': "Unknown error: " + str(e)})

    return plugins

'''
def get_block_yaml(block):
    tree = BeautifulSoup(block)
    values = None
    for node in tree.find_all('pre'):
        if len(node.text) > 0:
            try:
                values = parse_yaml(node.text) # yaml.load(node.text, Loader=CLoader)
                if type(values) is str:
                    print("Malformed yaml string ", values)
                    return "YAMLERROR: Malformed string"
            except Exception as e:
                print("Malformed yaml string ", str(e))
                return "YAMLERROR: Malformed string"
    return values
'''


def parse_plugin_values_old(nodes):
    plugins = []
    for node in nodes:
        values = {}
        name = node['plugin']

        if len(node.text) > 0:
            try:
                values = yaml.load(node.text) #, Loader=CLoader)
            except (yaml.parser.ParserError, yaml.scanner.ScannerError):
                plugins.append({"plugin": name, 'error': "YAML is malformed"})
        try:
            if type(values) is str:
                plugins.append({"plugin": name, 'error': "YAML is malformed"})
            else:
                plugins.append({"plugin": name, "markup": values, "taskId": node['id']})
        except KeyError:
            plugins.append({"plugin": name, 'error': "Missing identifier"})
    return plugins


class Node(dict):
    def __init__(self, plugin, text, id):
        self["plugin"] = plugin
        self.text = text
        self["id"] = id


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
byCode:||
eka
|
""")
    st2 = ("""
type: shell
path: user
rows: 1
byCode: javac example/Hello.java
""")
    markup = parse_yaml(st)
    print(markup)
    n1 = Node(plugin="n1", id="1", text=st)
    n2 = Node(plugin="p2", id="2", text=st2)
    plgs = parse_plugin_values([n1, n2])
    print(plgs)
    plgs = parse_plugin_values([n2])
    print(plgs)
    plgs = parse_plugin_values_old([n2])
    print(plgs)
    name = "/tmp/eka/toka/kolmas"
    print(os.path.dirname(name))