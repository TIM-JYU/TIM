# -*- coding:utf-8 -*-
from urllib.request import urlopen
import re
import html
import json
from urllib.parse import urlparse, parse_qs
import urllib
import pprint
import codecs
import bleach
import os

CACHE_DIR = "/tmp/cache/"

class QueryClass:
    def __init__(self):
        self.get_query = {}
        self.query = {}
        self.jso = None


def check_key(query, key):
    # return key   
    key2 = "-" + key;
    if key in query.query: return key
    if key2 in query.query: return key2
    if key in query.get_query: return key
    if key2 in query.get_query: return key2
    if not query.jso: return key
    if "input" in query.jso and "markup" in query.jso["input"]:
       if key in query.jso["input"]["markup"]: return key
       if key2 in query.jso["input"]["markup"]: return key2
    if "markup" not in query.jso: return key
    if key in query.jso["markup"]: return key
    if key2 in query.jso["markup"]: return key2
    return key


def get_param(query, key, default):
    key = check_key(query,key)
    dvalue = default
    if key in query.query: dvalue = query.query[key][0]
    if dvalue == 'undefined': 
        dvalue = default

    if key not in query.get_query:
        if query.jso is None: return dvalue
        if "input" in query.jso and "markup" in query.jso["input"] and key in query.jso["input"]["markup"]:
            value = query.jso["input"]["markup"][key]
            if value != 'undefined': return value

        if "markup" not in query.jso: return dvalue
        if key in query.jso["markup"]: return query.jso["markup"][key]
        return dvalue
    value = query.get_query[key][0]
    if value == 'undefined': return dvalue
    return value


def handle_by(byc):
    if not byc: return byc
    bycs = byc.split("\n")
    if len(bycs) > 0 and bycs[0].strip() == "//":  # remove empty comment on first line (due YAML limatations)
        del bycs[0]
        byc = "\n".join(bycs)
    n = len(byc)
    if n > 0 and byc[n - 1] == "\n": byc = byc[0:n - 1]
    return byc


def get_param_by(query, key, default):
    byc = get_param(query, key, default)
    # print("KEY: ", key, " PYC: ", byc, "|||")
    if not byc: byc = default
    if not byc: return byc
    byc = handle_by(byc)
    # print("KEY: ", key, " PYC: ", byc, "|||")
    return byc


def get_param_del(query, key, default):
    key = check_key(query,key)
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


def replace_param(query, key, new_value):
    if key not in query.query:
        if query.jso is None: return
        if "markup" not in query.jso: return
        if key in query.jso["markup"]:
            query.jso["markup"][key] = new_value
        return
    value = query.query[key][0]
    if value == 'undefined': return
    query.query[key][0] = new_value


def do_matcher(key):
    if not key:
        return False
    return re.compile(key)


def check(matcher, line):
    if not matcher:
        return False
    match = matcher.search(line)
    return match


def get_json_param(jso, key1, key2, default):
    try:
        if jso is None: return default
        if key1 not in jso: return default
        if not key2: return jso[key1]
        if not jso[key1]: return default
        if key2 not in jso[key1]: return default
        return jso[key1][key2]
    except:
        # print("JSO XXXXXXXXXXXXX", jso)
        print("KEY1=", key1, "KEY2=", key2)
        return default


def get_json_param3(jso, key1, key2, key3, default):
    try:
        if jso is None: return default
        if key1 not in jso: return default
        if not key2: return jso[key1]
        if not jso[key1]: return default
        if key2 not in jso[key1]: return default
        if key3 not in jso[key1][key2]: return default
        return jso[key1][key2][key3]
    except:
        # print("JSO XXXXXXXXXXXXX", jso)
        print("KEY1=", key1, "KEY2=", key2, "KEY3=", key3)
        return default


def get_scan_value(s):
    direction = 1
    if s:
        if s[0] == '-':
            direction = -1
            s = s[1:]
        if s[0] == '+':
            s = s[1:]
    match = do_matcher(s.replace("\\\\", "\\"))
    return direction, match


def scan_lines(lines, n, i, scanner, direction):
    if not scanner: return i
    i += direction
    while 0 <= i < n:
        line = lines[i]
        if check(scanner, line): return i
        i += direction
    if i < 0: i = 0
    if i >= n: i = n - 1
    return i


cache = {}


def clear_cache():
    global cache
    cache = {}
    for f in os.listdir(CACHE_DIR):
        os.remove(CACHE_DIR+f)


def get_chache_keys():
    global cache
    s = ""
    for key in cache.keys(): s += key + "\n"
    s = s + "\n".join(os.listdir(CACHE_DIR))
    return s


def get_url_lines(url):
    global cache
    # print("========= CACHE KEYS ==========\n", get_chache_keys())
    if url in cache: 
        # print("from cache: ", url)
        return cache[url]

    diskcache = CACHE_DIR+url.replace('/','_').replace(':','_')

    print("not in cache: ", url)
    # if False and os.path.isfile(diskcache):
    if os.path.isfile(diskcache):
        try:
            result = open(diskcache,"r",encoding='iso8859_15').read()
            result = result.split("\n");
            print("from DISK cache: ", diskcache)
            cache[url] = result

            return result
        except Exception as e:
            print(str(e))
            print("error reading filecache: ", diskcache)
            

    try:
        req = urlopen(url)
        # ftype = req.headers['content-type']
        lines = req.readlines()
    except:
        return False
    # filecontent = nltk.clean_html(html)

    n = len(lines)

    for i in range(0, n):
        line = lines[i]
        try:
            line = line.decode('utf-8-sig')
        except:
            try:
                line = line.decode(encoding='iso8859_15')
            except:
                line = str(line)
        lines[i] = line.replace("\n", "").replace("\r", "")

    cache[url] = lines

    try:
        # open(diskcache,"w").write("\n".join(lines));
        open(diskcache,"w",encoding='iso8859_15').write("\n".join(lines));
    except Exception as e:
        print(str(e))
        print("XXXXXXXXXXXXXXXXXXXXXXXX Could no write cache: \n",diskcache)

    
    
    return lines


def get_url_lines_as_string(url):
    global cache
    cachename = "lines_" + url
    diskcache = CACHE_DIR+cachename.replace('/','_').replace(':','_')
    # print("========= CACHE KEYS ==========\n", get_chache_keys())
    # print(cachename + "\n")
    #print(cache) # chache does not work in forkingMix
    if cachename in cache:
        # print("from cache: ", cachename)
        return cache[cachename]

    # print("not in cache: ", cachename)
    # return "File not found: " + url
    
    if os.path.isfile(diskcache):
        try:
            result = open(diskcache,"r").read()
            cache[cachename] = result
            return result
        except:
            print("error reading filecache: ", diskcache)
            
    # print("not in filecache: ", diskcache)

    try:
        req = urlopen(url)
        # ftype = req.headers['content-type']
        lines = req.readlines()
    except:
        return "File not found: " + url
    # filecontent = nltk.clean_html(html)

    n = len(lines)
    result = ""

    for i in range(0, n):
        line = lines[i]
        try:
            line = line.decode('utf-8-sig')
        except:
            try:
                line = line.decode(encoding='iso8859_15')
            except:
                line = str(line)
        result += line.replace("\r", "")

    result = result.strip("\n")
    cache[cachename] = result
    
    try:
        open(diskcache,"w").write(result)
    except:
        print("Could no write cache: ",diskcache)
    
    # print(cache)
    return result

    
def do_escape(s):
    line = html.escape(s)
    # line = line.replace("{","&#123;") # because otherwise problems with angular {{, no need if used inside ng-non-bindable
    # line = line.replace("}","&#125;") # because otherwise problems with angular }}
    return line


class FileParams:
    def __init__(self, query, nr, url, **defs):
        self.url = get_param(query, "file" + nr, "")  # defs.get('file',""))
        self.start = do_matcher(get_param(query, "start" + nr, "").replace("\\\\", "\\"))
        self.start_scan_dir, self.start_scan = get_scan_value(get_param(query, "startscan" + nr, ""))
        self.startcnt = int(get_param(query, "startcnt" + nr, "1"))
        self.startn = int(get_param(query, "startn" + nr, "0"))
        self.end = do_matcher(get_param(query, "end" + nr, "").replace("\\\\", "\\"))
        self.end_scan_dir, self.end_scan = get_scan_value(get_param(query, "endscan" + nr, ""))
        self.endcnt = int(get_param(query, "endcnt" + nr, "1"))
        self.endn = int(get_param(query, "endn" + nr, "0"))
        self.linefmt = get_param(query, "linefmt" + nr, "")
        self.maxn = int(get_param(query, "maxn" + nr, "10000"))
        self.lastn = int(get_param(query, "lastn" + nr, "1000000"))
        self.include = get_param(query, "include" + nr, "")
        self.replace = do_matcher(get_param(query, "replace" + nr, ""))
        self.by = get_param_by(query, "by" + nr, "")
        self.prorgam = get_param_by(query, "program" + nr, "")
        self.breakCount = int(get_param(query, "breakCount" + nr, "0"))

        self.reps = []

        repNr = ""
        if nr: repNr = nr + "."

        for i in range(1, 10):
            rep = do_matcher(
                get_param(query, "replace" + repNr + str(i), ""))  # replace.1.1 tyyliin replace1 on siis replace.0.1
            if not rep: break
            byc = get_param_by(query, "byCode" + repNr + str(i), "")
            self.reps.append({"by": rep, "bc": byc})

        if self.breakCount:
            self.breaks = []
            self.breaksBegin = []
            self.breaksBeforeBegin = []
            self.breaksBefore = []
            self.breaksAfter = []
            defAfter = -1
            for i in range(0, self.breakCount+1):
                brkdef = ""
                if i >= self.breakCount:
                    brkdef = "!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                    defAfter = 0
                self.breaksBegin.append(do_matcher(get_param(query, "breakBegin" + repNr + str(i), "").replace("\\\\", "\\")))
                self.breaks.append(do_matcher(get_param(query, "break" + repNr + str(i), brkdef).replace("\\\\", "\\")))
                self.breaksBeforeBegin.append(int(get_param(query, "breakBeforeBegin" + repNr + str(i), 0)))
                self.breaksBefore.append(int(get_param(query, "breakBefore" + repNr + str(i), 0)))
                self.breaksAfter.append(int(get_param(query, "breakAfter" + repNr + str(i), defAfter)))
                if defAfter < 0: defAfter = 0
                else: defAfter = -1


        usercode = get_json_param(query.jso, "input" + nr, "usercode", None)
        # if ( query.jso != None and query.jso.has_key("input") and query.jso["input"].has_key("usercode") ):
        if usercode:
            self.by = usercode
        else:
            usercode = get_json_param(query.jso, "state" + nr, "usercode", None)
            if usercode: self.by = usercode

        u = get_param(query, "url" + nr, "")
        if u and not self.url: self.url = url
        # if self.url: print("url: " + self.url + " " + self.linefmt + "\n")

    def get_file(self, escape_html=False):
        if self.prorgam:
            # print(self.prorgam)
            return self.scan_needed_lines(self.prorgam.split("\n"), escape_html)
        if not self.url:
            # print("SELF.BY:", self.by.encode());
            if not self.by: return ""
            return self.by # self.by.replace("\\n", "\n")

        lines = get_url_lines(self.url)
        if not lines: return "File not found " + self.url

        return self.scan_needed_lines(lines, escape_html)

    def get_raw_lines(self, escape_html=False):
        if self.prorgam:
            # print(self.prorgam)
            return self.prorgam.split("\n")
        if not self.url:
            # print("SELF.BY:", self.by.encode());
            if not self.by: return [];
            return [] # self.by.replace("\\n", "\n")

        lines = get_url_lines(self.url)
        if not lines: return ["File not found " + self.url];

        return lines

    def scan_needed_range(self, lines):
        n = len(lines)
        n1 = scan_lines(lines, n, 0, self.start, 1)
        n2 = n1
        n1 = scan_lines(lines, n, n1, self.start_scan, self.start_scan_dir)
        n1 += self.startn
        if n1 < 0: n1 = 0
        if n2 < n1: n2 = n1  # if n1 went forward

        if self.end:
            n2 = scan_lines(lines, n, n2, self.end, 1)
            n2 = scan_lines(lines, n, n2, self.end_scan, self.end_scan_dir)
        else:
            n2 = n - 1
        n2 += self.endn
        if n2 >= n: n2 = n - 1
        return n1, n2

    def scan_needed_lines(self, lines, escape_html=False):
        (n1,n2) = self.scan_needed_range(lines)
        ni = 0

        result = ""

        replace_by = self.by
        if replace_by:
            rep = replace_by.split("\n")
            if len(rep) > 0 and rep[0].strip() == "//":  # remove empty comment on first line (due YAML limatations)
                del rep[0]
                replace_by = "\n".join(rep)

        for i in range(n1, n2 + 1):
            line = lines[i]
            # if enc or True: line = line.decode('UTF8')
            # else: line = str(line)
            if check(self.replace, line): line = replace_by  # + "\n"
            for r in self.reps:
                if check(r["by"], line): line = r["bc"]  # + "\n"

            if escape_html: line = html.escape(line)
            ln = self.linefmt.format(i + 1)
            result += ln + line + "\n"
            if i + 1 >= self.lastn: break
            ni += 1
            if ni >= self.maxn: break

        return result


    def join_lines(self, lines, n1, n2, escape_html):
        result = ""
        n = len(lines)
        if n1 < 0: n1 = 0
        if n2 >= n: n2 = n -1
        for i in range(n1, n2 + 1):
            line = lines[i]
            #for r in self.reps:
            #    if check(r["by"], line): line = r["bc"]  # + "\n"

            if escape_html: line = html.escape(line)
            ln = self.linefmt.format(i + 1)
            result += ln + line + "\n"
            if i + 1 >= self.lastn: break

        return result



    def get_include(self, escapeHTML=False):
        if not self.include: return ""
        data = self.include.replace("\\n", "\n")
        if escapeHTML: data = html.escape(data)
        return data


    def scan_line_parts_range(self, part, n0, lines):
        n = len(lines)
        n1 = scan_lines(lines, n, n0+self.breaksBeforeBegin[part], self.breaksBegin[part], 1)
        n2 = n1
        # n1 = scan_lines(lines, n, n1, self.start_scan, self.start_scan_dir)
        n1 += self.breaksBefore[part]
        if n1 < 0: n1 = 0
        if n2 < n1: n2 = n1  # if n1 went forward

        if self.breaks[part]:
            n2 = scan_lines(lines, n, n2, self.breaks[part], 1)
        # n2 = scan_lines(lines, n, n2, self.end_scan, self.end_scan_dir)
        else:
            n2 = n - 1
        n2 += self.breaksAfter[part]
        if n2 >= n: n2 = n - 1
        return n1, n2

def get_params(self):
    result = QueryClass()
    result.get_query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    return result


def get_file_to_output(query, show_html):
    try:
        p0 = FileParams(query, "", "")
        # if p0.url == "":
        s = p0.get_file(show_html)
        # if not s:
        # return "Must give file= -parameter"
        s += p0.get_include(show_html)
        u = p0.url
        for i in range(1, 10):
            p = FileParams(query, "." + str(i), u)
            s += p.get_file(show_html)
            s += p.get_include(show_html)
            if p.url: u = p.url
        return s
    except Exception as e:
        return str(e)

def get_file_parts_to_output(query, show_html):
    p0 = FileParams(query, "", "")
    parts = [];
    n2 = -1
    lines = p0.get_raw_lines(show_html)
    j = 0
    part0 = "";
    s = get_param(query,"byCode","")
    if s: part0 = s
    s = usercode = get_json_param(query.jso, "input", "usercode", None)
    if s: part0 = s
    p0.reps.insert(0,{"by": "", "bc": part0});
    for i in range(0,p0.breakCount+1):
        n1, n2 = p0.scan_line_parts_range(i,n2+1,lines)
        part = p0.join_lines(lines,n1,n2,show_html)
        if i % 2 != 0:
            if j < len(p0.reps) and p0.reps[j] and p0.reps[j]["bc"]:
                part = p0.reps[j]["bc"]
            #else:
            #    p0.reps[j]["bc"] = part
            j += 1
        parts.append(part)
    return parts


def join_file_parts(p0,parts):
    s = ""
    for i in range(0,len(parts)):
        s = s + parts[i];

    return s

number_of_multi_html_reqs = 0


def multi_post_params(self):
    content_length = int(self.headers['Content-Length'])
    f = self.rfile.read(content_length)

    # print(f)
    # print(type(f))
    u = f.decode("UTF8")

    # uncomment to get request to file /tmps/mhx.json
    # global number_of_multi_html_reqs
    # number_of_multi_html_reqs += 1
    # with codecs.open("/tmps/mh%d.json" % number_of_multi_html_reqs, "w", "utf-8") as fj: fj.write(u)

    jsos = json.loads(u)
    results = []
    for jso in jsos:
        results.append(get_query_from_json(jso))
    return results


def get_query_from_json(jso):
    # print(jso.repr())
    # print("====================================================")
    result = QueryClass()
    # print("result.query ================================== ")
    # pp.pprint(result.query)
    # print jso
    result.jso = jso
    for field in list(result.jso.keys()):
        # print field + ":" + jso[field]
        if field == "markup":
            for f in list(result.jso[field].keys()):
                if f == "byCode":
                    result.query[f] = [handle_by(str(result.jso[field][f]))]
                else:
                    result.query[f] = [str(result.jso[field][f])]
        else:
            if field != "state" and field != "input": result.query[field] = [str(result.jso[field])]
    # print(jso)
    return result


def post_params(self):
    # print "postParams ================================================"
    # print self
    # pprint(self.__dict__,indent=2)
    # print dir(self.request)
    # print(self.path)
    # print(self.headers)
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
    # print(type(f))
    u = f.decode("UTF8")
    # print(u)
    # print(type(u))

    result = QueryClass()
    result.query = {}  # parse_qs(urlparse(self.path).query, keep_blank_values=True)
    # print("result.query ================================== ")
    # pp = pprint.PrettyPrinter(indent=4)
    # pp.pprint(result.query)

    if len(u) == 0: return result
    if content_type.find("json") < 0:  # ei JSON
        # print("POSTPARAMS============")
        q = parse_qs(urlparse('k/?' + u).query, keep_blank_values=True)
        for field in list(q.keys()):
            # print("FIELD=", field)
            result.query[field] = [q[field][0]]
    else:
        jso = json.loads(u)
        result = get_query_from_json(jso)

    result.get_query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    for f in result.get_query:
        result.query[f] = [result.get_query[f][0]]
    return result


def file_to_string(name):
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    result = ""
    for i in range(0, len(lines)):
        line = lines[i]
        result += line
    fr.close()
    return result


def query_params_to_angular(query):
    result = ""
    for field in query.keys():
        result = result + field + "=\"" + query[field][0] + "\";\n"
    # print "QUERY" + str(query)
    return result


def query_params_to_attribute(query, leave_away):
    result = ""
    # print("leave_away " + leave_away)
    for field in query.keys():
        if not (leave_away and field == leave_away):
            result = result + field.lower() + "=\'" + query[field][0] + "\'\n"
    # print "QUERY" + str(query)
    return result + ""


def query_params_to_map(query):
    result = {}
    for field in query.keys():
        if not field.startswith("-"): result[field] = query[field][0]

    return result


def query_params_to_map_check_parts(query):
    result = query_params_to_map(query.query)

    # Replace all byCode by fileparts if exists
    if int(get_param(query,"breakCount",0)) > 0:
        parts = get_file_parts_to_output(query,False)
        if not "byCode" in result and parts[1]:  result["byCode"] = parts[1]
        j = 1
        for i in range(3,len(parts)):
            if i % 2 != 0:
                byn = "byCode" + str(j)
                if not byn in result and parts[i]:  result[byn] = parts[i]
                j += 1
    return result


def query_params_to_json(query):
    result = json.dumps(query_params_to_map(query))
    return result


def file_to_string_replace_ng(name, what_to_replace, query):
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    result = ""
    params = query_params_to_angular(query.query)
    for i in range(0, len(lines)):
        line = lines[i].replace(what_to_replace, params)
        result += line
    fr.close()
    return result


def file_to_string_replace_url(name, what_to_replace, query):
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    # params = queryParamsToURL(query)
    qmap = query_params_to_map(query.query)
    params = urllib.parse.urlencode(qmap)
    result = ""
    for i in range(0, len(lines)):
        line = lines[i].replace(what_to_replace, params)
        result += line
    fr.close()


def string_to_string_replace_url(line, what_to_replace, query):
    qmap = query_params_to_map(query.query)
    params = urllib.parse.urlencode(qmap)
    line = line.replace(what_to_replace, params)
    height = get_param(query, "height", "100%")
    line = line.replace('##HEIGHT##', str(height))
    return line


def file_to_string_replace_attribute(name, what_to_replace, query):
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    params = query_params_to_attribute(query.query)
    result = ""
    for i in range(0, len(lines)):
        line = lines[i].replace(what_to_replace, params)
        result += line
    fr.close()
    return result


def string_to_string_replace_attribute(line, what_to_replace, query):
    leave_away = None
    if "##USERCODE##" in line: leave_away = "byCode"
    params = query_params_to_attribute(query.query, leave_away)
    line = line.replace(what_to_replace, params)
    by = get_param_by(query, "byCode", "")
    by = get_param_by(query, "usercode", by)
    # print("BY XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", by.encode())
    line = line.replace("##USERCODE##", by)
    # print(line.encode())
    return line


def allow(s):
    tags = ['em', 'strong', 'tt', 'a', 'b', 'code', 'i', 'kbd']
    attrs = {
        'a': ['href']
    }
    return bleach.clean(s, tags, attrs)


def clean(s):
    s = str(s)
    s = s.replace('"', '')  # kannattaako, tällä poistetaan katkaisun mahdollisuus?
    return s
    # return bleach.clean(s)


attrs = {
        '*': ['id', 'class'],
        'a': ['href']
}
tags = ['a', 'p', 'em', 'strong', 'tt', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'i', 'b', 'code']


def get_heading(query, key, def_elem):
    if not query: return ""
    # return "kana"
    h = get_param(query, key, None)
    # print("h=",h)
    if not h: return ""
    h = str(h)
    return "<" + def_elem + ">" + h + "</" + def_elem + ">\n"

    # Loppu on liian hidasta koodia, kannattaa muuten vaihtaa järjestys jos tuota vielä käyttää, samoin js-koodissa
    st = h.split("!!")  # h4 class="h3" width="23"!!Tehtava 1
    elem = def_elem
    val = st[0]
    attributes = ""
    if len(st) >= 2:
        elem = st[0]
        val = st[1]
    i = elem.find(' ')
    ea = [elem]
    if i >= 0:
        ea = [elem[0:i], elem[i:]]
    if len(ea) > 1:
        elem = ea[0]
        attributes = ea[1] + " "
    val = allow(val)
    result_html = "<" + elem + attributes + ">" + val + "</" + elem + ">\n"
    # result_html = bleach.clean(result_html, tags, attrs)
    return result_html


def get_surrounding_headers2(query):
    result = get_heading(query, "header", "h4")
    stem = allow(get_param(query, "stem", None))
    if stem: result += '<p class="stem" >' + stem + '</p>'
    return result, get_heading(query, "footer", 'p class="plgfooter"')


def get_surrounding_headers(query, inside):
    result = get_heading(query, "header", "h4")
    stem = allow(get_param(query, "stem", None))
    if stem: result += '<p class="stem" >' + stem + '</p>\n'
    result += inside + '\n'
    result += get_heading(query, "footer", 'p class="plgfooter"')
    return result


def get_clean_param(query, key, default):
    s = get_param(query, key, default)
    # return str(s)
    return clean(s)


def do_headers(self, content_type):
    self.send_response(200)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
    self.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
    self.send_header('Content-type', content_type)
    self.end_headers()

    
# Etsii paketin ja luokan nimen tiedostosta    
def find_java_package(s):
    p = ""
    c = ""
    r = re.search("package\s*([\s\.a-zA-Z0-9_]+)", s, flags=re.M)
    if r: p = re.sub(r"\s*", "", r.group(1))
    r = re.search("public\s*class\s*([a-zA-Z0-9_]+)", s, flags=re.M)
    if r: c = r.group(1)
    else:
        r = re.search("public\s*interface\s*([a-zA-Z0-9_]+)", s, flags=re.M)
        if r: c = r.group(1)

    return p, c


#Palauttaa intin joka löytyy jonon alusta    
def getint(s):
    i = 0
    s = s.strip(" ")
    while i < len(s):
        if "0123456789".find(s[i:i+1]) < 0:
            if i == 0:
                return 0
            return int(s[0:i])
        i += 1
    return int(s)


import hashlib
import binascii


# see: https://docs.python.org/3/library/hashlib.html
def hash_user_dir(user_id):
    dk = hashlib.pbkdf2_hmac('sha256', str.encode(user_id), b"tim", 100)
    return bytes.decode(binascii.hexlify(dk))


# Korvataan sisällössä scriptit    
def replace_scripts(s,scripts,placeholder):
    sc = ""
    if scripts != "":
        scs = scripts.split(",")
        for s1 in scs:
            sc += '<script src="'+s1+'"></script>\n'
    return s.replace(placeholder,sc)    
    
    
def get_templates(dirname: str) -> object:
    """
    Find all templates from dirname.  Each template file should include
    template text - first line
    template explanation - second line
    template content - rest of lines
    :param dirname: the directory name where to find template files
    :return: list of template items, one item is file: text: explanation
    """
    result = []
    for filename in os.listdir(dirname):
        f = open(dirname+"/"+filename, encoding="utf-8-sig").readlines()
        template = {"file": filename, "text": f[0].strip(),"expl": f[1].strip() }
        result.append(template)

    return result


def get_all_templates(dirname: str) -> object:
    """
    Find list of all templates from dirname.  Dir should include
    file tabs.txt where there is one line for every tab needed for TIM editor.
    Then there should be directories 0, 1, ... for each corresponding
    tab-line.  So if tehre is two lines in tabs.txt tehre is directories 0 and 1
    for first and second tab.
    :param dirname: the directory name where to find template list file and sub directories for templates
    :return: dict with list of lif to template items (templates) and texts (text)
    """
    templates = []
    texts = []
    try:
        texts = open(dirname+"/tabs.txt", encoding="utf-8-sig").read().splitlines();
        for i in range(0, len(texts)):
            templates.append(get_templates(dirname+"/"+str(i)))
    except Exception as e:
        print(e)
        return {}
    return {'templates': templates, 'text': texts}


def get_template(dirname: str, idx:str, filename: str) -> str:
    """
    Returns the template file from line 2 to end of file
    :param dirname: from directory (be sure this is valid)
    :param idx: index for the template  (only numbers allowed)
    :param filename: from file (validity of this is checked)
    :return: lines starting from line 2.
    """
    try:
        fname = re.sub(r"[^ A-ZÅÄÖa-zåäö_0-9]","",filename)
        if not fname: fname = "0"
        tidx = re.sub(r"[^0-9]","",idx)
        f = open(dirname+"/"+tidx+"/"+fname, encoding="utf-8-sig").readlines()
    except Exception as e:
        return str(e)
    return "".join(f[2:])

    
def join_dict(a: dict,b: dict):
    """
    Joins two dict and returns a new one
    :param a: first dict to join
    :param b: next dict to join
    :return: "a+b"
    """
    result = a.copy()
    result.update(b)
    return result

    
LAZYSTART="<!--lazy "
LAZYEND =" lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"
 
 
def is_lazy(query):
    # print(query)
    caller_lazy = get_param(query, "doLazy", NEVERLAZY)
    # print("caller_lazy=",caller_lazy)
    if caller_lazy == NEVERLAZY: return False
    do_lazy = caller_lazy
    if str(do_lazy).lower() == "true":  do_lazy = True
    if str(do_lazy).lower() == "false": do_lazy = False
    lazy = get_param(query, "lazy", "")
    if str(lazy).lower() == "true":  do_lazy = True
    if str(lazy).lower() == "false": do_lazy = False
    # print("do_lazy=",do_lazy)
    return do_lazy  
    
    
def is_user_lazy(query):
    caller_lazy = get_param(query, "doLazy", NEVERLAZY)
    # print("caller_lazy=",caller_lazy)
    if caller_lazy == NEVERLAZY: return False
    do_lazy = False
    if str(caller_lazy).lower() == "false": return False
    lazy = get_param(query, "lazy", "")
    if str(lazy).lower() == "true":  do_lazy = True
    # print("do_lazy=",do_lazy)
    return do_lazy


def add_lazy(plugin_html: str) -> str:
    return LAZYSTART + plugin_html + LAZYEND


def make_lazy(plugin_html: str, query, htmlfunc) -> str:
    """
    Makes plugin string to lazy
    :param plugin_html: ready html for the plugin
    :param query: query params where lazy options can be read 
    :param htmlfunc: function to generate the lazy version of plugin html
    :return true if lazy plugin is needed
    """
    if not is_lazy(query): return plugin_html
    lazy_html = htmlfunc(query)
    lazy_plugin_html = LAZYSTART + plugin_html + LAZYEND + lazy_html
    return lazy_plugin_html
    
    
def replace_template_params(query, template: str, cond_itemname: str, itemnames=None) -> str:
    """
    Replaces all occurances of itemnames and cond_item_name in template by their value in query
    if  cond_itemname exists in query. 
    :param query: query params where items can be read 
    :param template: string that may include items like {{userword}} that are replaced
    :param cond_itemname: name for the item that decides if the template is non empty.  None means no condition
    :param itemnames: list of other item names that are replaced by their value in Query
    :return true if lazy plugin is needed
    """
    items = [] 
    if cond_itemname:
        item = get_param(query,cond_itemname, "")
        if not item: return ""
        items = [cond_itemname]
     
    if itemnames: items += itemnames
    result = template
       
    for name in items:
        n,d,dummy = (name+"::").split(":",2)
        item = str(get_param(query, n, d))
        result = result.replace("{{"+n+"}}", item)
        
    return result    
        

def replace_template_param(query, template: str, cond_itemname: str, default="") -> str:
    """
    Replaces all occurances of itemnames and cond_item_name in template by their value in query
    if  cond_itemname exists in query.
    :param query: query params where items can be read
    :param template: string that may include items like {{userword}} that are replaced
    :param cond_itemname: name for the item that decides if the template is non empty.  None means no condition
    :param default: value to be used if item is default or no item at all, if defult=="", return "" if used
    :return replaced template or ""
    """
    items = []
    if not cond_itemname: return ""
    item = get_param(query, cond_itemname, "default")
    if item == "default": item = default
    if item == "": return ""
    if not item: return ""

    result = template.replace("{{"+cond_itemname+"}}", str(item))

    return result

