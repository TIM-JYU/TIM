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


class QueryClass:
    def __init__(self):
        self.get_query = {}
        self.query = {}
        self.jso = None


def get_param(query, key, default):
    dvalue = default
    if key in query.query: dvalue = query.query[key][0]
    if dvalue == 'undefined': dvalue = default

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
    if n > 0  and byc[n-1] == "\n": byc = byc[0:n-1]
    return byc;   
  
def get_param_by(query, key, default):
    byc = get_param(query, key, default)
    print("KEY: ",key," PYC: ",byc,"|||")
    if not byc: byc = default
    if not byc: return byc
    byc = handle_by(byc)
    print("KEY: ",key," PYC: ",byc,"|||")
    return byc
    

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


def get_chache_keys():
    global cache
    s = ""
    for key in cache.keys(): s += key + "\n"
    return s

def get_url_lines(url):
    global cache
    # print("========= CACHE KEYS ==========\n", get_chache_keys())
    if url in cache: return cache[url]

    try:
        req = urlopen(url)
        ftype = req.headers['content-type']
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
    return lines


class FileParams:
    def __init__(self, query, nr, url):
        self.url = get_param(query, "file" + nr, "")
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

        self.reps = []

        repNr = ""
        if nr: repNr = nr+"."
        
        for i in range(1, 10):
            rep = do_matcher(get_param(query, "replace" + repNr + str(i), "")) #  replace.1.1 tyyliin replace1 on siis replace.0.1
            if not rep: break
            byc = get_param_by(query, "byCode" + repNr + str(i), "")
            self.reps.append({"by": rep, "bc": byc})


        usercode = get_json_param(query.jso, "input" + nr, "usercode", None)
        # if ( query.jso != None and query.jso.has_key("input") and query.jso["input"].has_key("usercode") ):
        if usercode:
            self.by = usercode
        else:
            usercode = get_json_param(query.jso, "state" + nr, "usercode", None)
            if usercode: self.by = usercode

        u = get_param(query, "url" + nr, "")
        if u and not self.url: self.url = url
        if self.url: print("url: " + self.url + " " + self.linefmt + "\n")

    def get_file(self, escape_html=False):
        if not self.url:
            # print("SELF.BY:", self.by.encode());
            if not self.by: return ""
            return self.by.replace("\\n", "\n")

        lines = get_url_lines(self.url)
        if not lines: return "File not found " + self.url

        return self.scan_needed_lines(lines, escape_html)

    def scan_needed_lines(self, lines, escape_html=False):
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


    def get_include(self, escapeHTML=False):
        if not self.include:  return ""
        data = self.include.replace("\\n", "\n")
        if escapeHTML: data = html.escape(data)
        return data


def get_params(self):
    result = QueryClass()
    result.get_query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    return result


def get_file_to_output(query, show_html):
    s = ""
    p0 = FileParams(query, "", "")
    # if p0.url == "":
    s = p0.get_file(show_html)
    # if not s:
    #    return "Must give file= -parameter"
    s += p0.get_include(show_html)
    u = p0.url
    for i in range(1, 10):
        p = FileParams(query, "."+str(i), u)
        s += p.get_file(show_html)
        s += p.get_include(show_html)
        if p.url: u = p.url
    return s


def multi_post_params(self):
    content_length = int(self.headers['Content-Length'])
    f = self.rfile.read(content_length)
    # print(f)
    # print(type(f))
    u = f.decode("UTF8")
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
                if f == "byCode": result.query[f] = [handle_by(str(result.jso[field][f]))]
                else: result.query[f] = [str(result.jso[field][f])]
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
        result[field] = query[field][0]
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
    s = s.replace('"', '')  # kannattaako, tällä poistetaam katkaisun mahdollisuus?
    return bleach.clean(s)


def get_heading(query, key, def_elem):
    if not query: return ""
    h = get_param(query, key, None)
    if not h: return ""
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
    attrs = {
        '*': ['id', 'class'],
        'a': ['href']
    }
    tags = ['a', 'p', 'em', 'strong', 'tt', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'i', 'b', 'code']
    result_html = bleach.clean(result_html, tags, attrs)
    return result_html


def get_surrounding_headers(query, inside):
    result = get_heading(query, "header", "h4")
    stem = allow(get_param(query, "stem", None))
    if stem:  result += '<p class="stem" >' + stem + '</p>\n'
    result += inside + '\n'
    result += get_heading(query, "footer", 'p class="footer"')
    return result


def get_clean_param(query, key, default):
    s = get_param(query, key, default)
    return clean(s)