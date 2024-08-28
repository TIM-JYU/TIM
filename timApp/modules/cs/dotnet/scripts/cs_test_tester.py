#!/usr/bin/env python3
# NUnit test validator for csplugin tasks
import json
import os
import re
import sys
from subprocess import call, DEVNULL


def replace_all(lines, s1, s2):
    for i in range(len(lines)):
        s = lines[i]
        lines[i] = re.sub(s1, s2, lines[i])


def replace_by(lines, instructions):
    replace = instructions.get("replace", None)
    if not replace:
        return
    for cond in replace:
        s1 = cond.get("sub", "")
        if not s1:
            continue
        s2 = cond.get("by", "")
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
            i1 = i - 1
        else:
            if i1 >= 0 and s.find("{") >= 0:
                n += 1
            if i1 >= 0 and s.find("}") >= 0:
                i2 = i
                n -= 1
                if n <= 0:
                    break

    return i1, i2


def replace_tests(lines, test):
    n = 1
    for t in test:
        i1, i2 = find_test(lines, t.get("replaceline", None))
        if i1 < 0 or i2 < 0:
            continue
        tlines = lines[i1 : i2 + 1]
        del lines[i1 : i2 + 1]
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
            name = tst.get("call", "XXXX")
            result = tst.get("result", "XXXX")
            # NUnit 3 uses "Passed" instead of "Success" and "Failed" instead of "Failure"
            if result == "Success":
                result = "Passed"
            elif result == "Failure":
                result = "Failed"
            expl = tst.get("expl", "???")
            pts = tst.get("pts", 1)
            line = [s for s in lines if s.find(name + "xxxx") >= 0]
            if line:
                line = line[0]
                rst = 'result="'
                i = line.find(rst)
                if i >= 0:
                    line = line[i + len(rst) :]
                    i = line.find('"')
                    if i >= 0:
                        xmlres = line[:i]
                        if xmlres == result:
                            p += pts
                        else:
                            pts = tst.get("wrong", 0)
                            p += pts
                        print(
                            expl
                            + ": pitäisi tulla "
                            + result
                            + ", tuli: "
                            + xmlres
                            + ". Pisteitä:",
                            pts,
                        )
    return p


def scale_points(pts, points):
    if not points:
        return pts
    p = 0
    for pt in points:
        if pts < pt.get("from", 0):
            return p
        p = pt.get("p", pts)
    return p


GLOBAL_NUGET_PACKAGES_PATH = "/cs_data/dotnet/nuget_cache"


def get_build_refs(ref_type):
    with open(f"/cs_data/dotnet/configs/{ref_type}.build.deps", encoding="utf-8") as f:
        dep_paths = [
            os.path.join(GLOBAL_NUGET_PACKAGES_PATH, dep_line.strip())
            for dep_line in f.readlines()
        ]
        return [f"-r:{p}" for p in dep_paths]


def _csharp_get_additional_deps(dep_files):
    return ":".join(f"/cs_data/dotnet/configs/{d}.deps.json" for d in dep_files)


def main():
    filename = sys.argv[1]
    filename2 = sys.argv[2]
    filename3 = "T" + filename
    lines = open(filename).readlines()
    lines2 = open(filename2).read()
    # yaml
    # instructions = yaml.load(lines2, CLoader)
    # insert = instructions.get("insert", None)

    # json
    instructions = json.loads(lines2)
    insertfile = instructions.get("insert", None)
    insert = ""
    if insertfile:
        insert = open(insertfile).read()

    replace_by(lines, instructions)

    replace_tests(lines, instructions.get("test", None))

    # print("".join(lines))
    # print(insert)

    f = open(filename3, "w")
    f.writelines(lines)
    if insert:
        f.write(insert)
    f.close()
    args1 = [
        "/cs/dotnet/csc",
        "-nologo",
        f"-out:{filename3}.dll",
        "-target:library",
        *get_build_refs("nunit_test"),
        *get_build_refs("jypeli"),
        filename3,
    ]

    sourceFiles = instructions.get("sourceFiles", [])
    for sourceFile in sourceFiles:
        args1.append(sourceFile)

    ret = call(args1)

    # print(ret)
    # print(args1)
    if ret != 0:
        print("Testikoodi ei käänny")
        return

    args = [
        "/cs/dotnet/nunit-test-dll",
        _csharp_get_additional_deps(["nunit_test", "code_analysis", "jypeli"]),
        f"{filename3}.dll",
    ]
    ret = call(args, stdout=DEVNULL, stderr=DEVNULL, timeout=20)

    # https://docs.nunit.org/articles/nunit/running-tests/Console-Runner.html
    # print(args)
    if ret < 0:
        print("Testikoodia ei voi ajaa")

    xml = open("TestResult.xml").readlines()
    # print("\n".join(xml))

    points = count_points(xml, instructions.get("test", None))
    points = scale_points(points, instructions.get("points", None))
    print("Points: " + f"{points:.2f}")


if __name__ == "__main__":
    main()
