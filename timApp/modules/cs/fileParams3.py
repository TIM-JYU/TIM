# -*- coding:utf-8 -*-
from urllib.request import urlopen
import re
import html
import cgi
import json
from urllib.parse import urlparse, parse_qs
from pprint import pprint
import urllib
import codecs


class QueryClass:
    query = {}
    jso = None


def get_param(query, key, default):
    if key not in query.query:
        if query.jso is None: return default
        if "markup" not in query.jso: return default
        if key in query.jso["markup"]: return query.jso["markup"][key]
        return default
    value = query.query[key][0]
    if value == 'undefined': return default
    return value


def get_param_del(query, key, default):
    if key not in query.query:
        if query.jso is None: return default
        if "markup" not in query.jso: return default
        if key in query.jso["markup"]:
            value = query.jso["markup"][key]
            del query.jso["markup"][key]
            return value
        return default
    value = query.query[key][0]
    del query.query[key]
    if value == 'undefined': return default
    return value


def replace_param(query, key, newValue):
    if key not in query.query:
        if query.jso is None: return
        if "markup" not in query.jso: return
        if key in query.jso["markup"]:
            query.jso["markup"][key] = newValue
        return
    value = query.query[key][0]
    if value == 'undefined': return
    query.query[key][0] = newValue


def do_matcher(key):
    if not key:
        return False
    return re.compile(key)


def check(matcher, line):
    if not matcher:
        return False
    match = matcher.search(line)
    return match


def getJSOParam(jso, key1, key2, default):
    try:
        if jso is None: return default
        if key1 not in jso: return default
        if not key2: return jso[key1]
        if not jso[key1]: return default
        if key2 not in jso[key1]: return default
        return jso[key1][key2];
    except:
        # print("JSO XXXXXXXXXXXXX", jso)
        print("KEY1=", key1, "KEY2=", key2)
        return default


class FileParams:
    def __init__(self, query, nr, url):
        self.url = get_param(query, "file" + nr, "")
        self.start = do_matcher(get_param(query, "start" + nr, "").replace("\\\\", "\\"))
        self.startcnt = int(get_param(query, "startcnt" + nr, "1"))
        self.startn = int(get_param(query, "startn" + nr, "0"))
        self.end = do_matcher(get_param(query, "end" + nr, "").replace("\\\\", "\\"))
        self.endcnt = int(get_param(query, "endcnt" + nr, "1"))
        self.endn = int(get_param(query, "endn" + nr, "0"))
        self.linefmt = get_param(query, "linefmt" + nr, "")
        self.maxn = int(get_param(query, "maxn" + nr, "10000"))
        self.lastn = int(get_param(query, "lastn" + nr, "1000000"))
        self.include = get_param(query, "include" + nr, "")
        self.replace = do_matcher(get_param(query, "replace" + nr, ""))
        self.by = get_param(query, "by" + nr, "")

        usercode = getJSOParam(query.jso, "input" + nr, "usercode", None);
        # if ( query.jso != None and query.jso.has_key("input") and query.jso["input"].has_key("usercode") ):
        if usercode: self.by = usercode

        u = get_param(query, "url" + nr, "")
        if u and not self.url: self.url = url
        if self.url: print("url: " + self.url + " " + self.linefmt + "\n")

    def printFile(self, file, enc=True, escapeHTML=False):
        if not self.url:
            if not self.by: return
            file.write(self.by.replace("\\n", "\n").encode())
            return
        try:
            lines = urlopen(self.url).readlines()
        except:
            return
        # filecontent = nltk.clean_html(html)
        startcnt = self.startcnt
        endcnt = self.endcnt
        doprint = not self.start
        n = len(lines)
        n1 = n
        n2 = n
        if doprint: n1 = 0

        for i in range(0, n):
            line = lines[i].decode('utf-8-sig').replace("\n","")
            lines[i] = line
            # print(i,": ",line)
            if not doprint and check(self.start, line):
                startcnt -= 1
                # print "startcnt {0} endcnt {1}".format(startcnt,endcnt)
                if startcnt <= 0:
                    doprint = True
                    n1 = i
            if doprint and check(self.end, line):
                endcnt -= 1
                if endcnt <= 0:
                    n2 = i
                    break

        n1 += self.startn
        n2 += self.endn
        if n1 < 0: n1 = 0
        if n2 >= n: n2 = n - 1

        ni = 0

        replaceBy = self.by
        if replaceBy:
            rep = replaceBy.split("\n");
            if len(rep) > 0 and rep[0].strip() == "//":
                del rep[0]
                replaceBy = "\n".join(rep)

        for i in range(n1, n2 + 1):
            line = lines[i]
            # if enc or True: line = line.decode('UTF8')
            # else: line = str(line)
            if check(self.replace, line):  line = replaceBy + "\n"
            if escapeHTML: line = html.escape(line)
            ln = self.linefmt.format(i + 1)
            nln = ln + line + "\n"
            if enc: file.write(nln.encode("UTF8"))
            else: file.write(nln)
            if i + 1 >= self.lastn: break
            ni += 1
            if ni >= self.maxn: break


    def printInclude(self, file, enc=True, escapeHTML=False):
        if not self.include: return
        data = self.include.replace("\\n", "\n")
        if escapeHTML: data = html.escape(data)
        if enc: file.write(data.encode())
        else: file.write(data)


def getParams(self):
    result = QueryClass()
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    return result


def postParams(self):
    # print "postParams ================================================"
    # print self
    # pprint(self.__dict__,indent=2)
    # print dir(self.request)
    print(self.path)
    print(self.headers)
    content_length = int(self.headers['Content-Length'])
    content_type = "application/json"
    if 'Content-Type' in self.headers: content_type = self.headers['Content-Type']
    if 'content-cype' in self.headers: content_type = self.headers['content-type']
    '''
    form = cgi.FieldStorage(
        fp=self.rfile,
        headers=self.headers,
        environ={'REQUEST_METHOD': 'POST',
                 'CONTENT_TYPE': content_type
        })
    '''

    f = self.rfile.read(content_length)
    print(f)
    print(type(f))
    u = f.decode("UTF8")
    print(u)
    print(type(u))

    result = QueryClass()
    result.query = {}  # parse_qs(urlparse(self.path).query, keep_blank_values=True)

    if len(u) == 0: return result
    if content_type.find("json") < 0:  # ei JSON
        print("POSTPARAMS============")
        q = parse_qs(urlparse('k/?'+u).query, keep_blank_values=True)
        for field in list(q.keys()):
            print("FIELD=",field)
            result.query[field] = [q[field][0]]
        return result

    jso = json.loads(u)
    # print(jso.repr())
    print("====================================================")
    result = QueryClass()
    # print jso
    result.jso = jso
    for field in list(result.jso.keys()):
        # print field + ":" + jso[field]
        if field == "markup":
            for f in list(result.jso[field].keys()):
                result.query[f] = [str(result.jso[field][f])]
        else:
            if field != "state": result.query[field] = [str(result.jso[field])]
    return result


'''
    # print self.request.body
    # print self.rfile
    # print json.dumps(form)
    # form = cgi.FieldStorage()
    # print dir(form)
    result = QueryClass()
    pprint(form.__dict__, indent=2)
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    # if ( form['type'].find('json') >= 0 ):
    if form.list is None:  # Onko JSON vai tavallinen POST
        s = str(form)
        i = s.find('{')
        i2 = s.rfind('\'')
        js = s[i:i2]
        js = js.replace("\\\\", "\\")
        print("js:======================\n")
        print(js)
        print("\n======================\n")
        result.jso = json.loads(js)
        # print jso
        for field in list(result.jso.keys()):
            # print field + ":" + jso[field]
            if field == "markup":
                for f in list(result.jso[field].keys()):
                    result.query[f] = [str(result.jso[field][f])]
            else:
                if field != "state": result.query[field] = [str(result.jso[field])]
        return result
    # print form
    # print "DATA: "
    # print form["data"]
    # print form.keys()
    # print "Environ: "
    # print form.environ
    for field in list(form.keys()):
        result.query[field] = [form[field].value]
    return result
'''


def printFileTo(name, f):
    fr = open(name, encoding="utf-8")
    lines = fr.readlines()
    for i in range(0, len(lines)):
        line = lines[i]
        f.write(line.encode())
    fr.close()


def queryParamsToNG(query):
    result = ""
    for field in query.keys():
        result = result + field + "=\"" + query[field][0] + "\";\n"
    # print "QUERY" + str(query)
    return result


def queryParamsToAttribute(query, leaveAway):
    result = ""
    # print("leaveAway " + leaveAway)
    for field in query.keys():
        if not (leaveAway and field == leaveAway):
            result = result + field.lower() + "=\'" + query[field][0] + "\'\n"
    # print "QUERY" + str(query)
    return result + ""


def queryParamsToMap(query):
    result = {}
    for field in query.keys():
        result[field] = query[field][0]
    return result


def printFileToReplaceNG(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    params = queryParamsToNG(query.query)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line.encode())
    fr.close()


def printFileToReplaceURL(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    # params = queryParamsToURL(query)
    qmap = queryParamsToMap(query.query)
    params = urllib.parse.urlencode(qmap)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line.encode())
    fr.close()


def printStringToReplaceURL(line, f, whatToReplace, query):
    qmap = queryParamsToMap(query.query)
    params = urllib.parse.urlencode(qmap)
    line = line.replace(whatToReplace, params)
    height = get_param(query, "height", "100%")
    line = line.replace('##HEIGHT##', height)
    f.write(line.encode())


def printFileToReplaceAttribute(name, f, whatToReplace, query):
    fr = open(name, "r")
    lines = fr.readlines()
    params = queryParamsToAttribute(query.query)
    for i in range(0, len(lines)):
        line = lines[i].replace(whatToReplace, params)
        f.write(line)
    fr.close()


def printStringToReplaceAttribute(line, f, whatToReplace, query):
    leaveAway = None;
    if "##USERCODE##" in line: leaveAway = "byCode"
    params = queryParamsToAttribute(query.query, leaveAway)
    line = line.replace(whatToReplace, params)
    line = line.replace("##USERCODE##", get_param(query, "byCode", ""))
    f.write(line.encode())
    print(line.encode())

