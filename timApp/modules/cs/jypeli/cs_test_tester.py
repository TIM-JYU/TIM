#!/usr/bin/env python3
# encoding: utf-8
import sys
from subprocess import call
import re
# import yaml
# from yaml import CLoader, YAMLError
import json
import shlex
from subprocess import Popen, PIPE

def replace_all(lines, s1, s2):
    for i in range(len(lines)):
        s = lines[i]
        lines[i] = re.sub(s1, s2, lines[i])


def replace_by(lines, instructions):
    replace = instructions.get("replace",None)
    if not replace:
        return
    for cond in replace:
        s1 = cond.get("sub","")
        if not s1:
            continue
        s2 = cond.get("by","")
        replace_all(lines, s1, s2)


def find_test(lines, testname):
    if not testname:
        return -1, -1
    reg = re.compile(testname)
    i1 = i2 = -1
    n = 0
    for i in range(len(lines)):
        s = lines[i]
        res = reg.match(s)
        if res:
            i1 = i-1
        else:
            if i1 >= 0 and s.find("{") >= 0:
                n += 1
            if i1 >= 0 and s.find("}") >= 0:
                i2 = i
                n -= 1
                if n <= 0:
                    break;

    return i1, i2


def replace_tests(lines, test):
    n = 1
    for t in test:
        i1, i2 = find_test(lines, t.get("replaceline", None))
        if i1 < 0 or i2 < 0:
            continue
        tlines = lines[i1: i2+1]
        del lines[i1: i2+1]
        replacecall = t.get("replacecall", "")
        byline = t.get("byline", "")

        for tst in t.get("bycalls", []):
            tmethod = list(tlines)
            tc = tst.get("call", "")
            tr = tst.get("result", "")
            if byline:
                tst["name"] = tc + "xxxx" + str(n)
                tmethod[1] = byline + tst["name"] + "()\n"
            n += 1
            # replace_all(tmethod, replacecall, tr)
            replace_all(tmethod, replacecall, tc)
            lines[i1:i1] = tmethod


def count_points(lines, test):
    p = 0
    for t in test:
        for tst in t.get("bycalls", []):
            name = tst.get("name", "XXXX")
            result = tst.get("result", "XXXX")
            expl = tst.get("expl", "???")
            pts = tst.get("pts", 1)
            line = [s for s in lines if s.find(name) >= 0]
            if line:
                line = line[0]
                rst = 'result="'
                i = line.find(rst)
                if i >= 0:
                    line = line[i+len(rst):]
                    i = line.find('"')
                    if i >= 0:
                        line = line[:i]
                        if line == result:
                            p += pts
                        else:
                            pts = tst.get("wrong", 0)
                            p += pts
                        print(expl + ": pitäisi tulla " + result + ", tuli: " + line + ". Pisteitä:", pts)
    return p


def scale_points(pts, points):
    if not points:
        return pts
    p = 0
    for pt in points:
        if pts < pt.get("from",0):
            return p
        p = pt.get("p", pts)
    return p


def main():
    filename = sys.argv[1]
    filename2 = sys.argv[2]
    filename3 = "T" + filename
    lines = open(filename, 'r').readlines()
    lines2 = open(filename2, 'r').read()
    # yaml
    # instructions = yaml.load(lines2, CLoader)
    # insert = instructions.get("insert", None)

    # json
    instructions = json.loads(lines2)
    insertfile = instructions.get("insert", None)
    insert = ""
    if insertfile:
        insert = open(insertfile, 'r').read()

    replace_by(lines, instructions)

    replace_tests(lines, instructions.get("test", None))

    # print("".join(lines))

    f = open(filename3, "w")
    f.writelines(lines)
    if insert:
        f.write(insert)
    f.close()
    # ret = call("echo Hello World", shell=True)
    ret = call("csc /nologo /out:" + filename3 + ".dll /target:library /r:System.Numerics.dll " +
               "/reference:/usr/lib/mono/gac/nunit.framework/2.6.4.0__96d09a1eb7f44a77/nunit.framework.dll " +
               filename3, shell=True)
    # print(ret)
    # ret = call("nunit-console -nologo -nodots " + filename3 + ".dll", shell=True)
    if ret != 0:
        print("Testikoodi ei käänny")
        return

    ## ret = call("nunit-console -nologo -xmlConsole " + filename3 + ".dll", shell=True)
    args = ["nunit-console", "-nologo", "-xmlConsole", filename3 + ".dll"]
    # args = ["nunit-console", filename3 + ".dll"]
    p = Popen(args, shell=False, stdout=PIPE, stderr=PIPE)
    stdout, stderr = p.communicate(timeout=20)

    xml = stdout.decode().split("\n")
    # print("\n".join(xml))

    points = count_points(xml, instructions.get("test", None))
    points = scale_points(points, instructions.get("points", None))
    print("Points: " + '{0:.2f}'.format(points))


if __name__ == "__main__":
    main()
