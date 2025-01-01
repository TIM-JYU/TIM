import base64
import binascii
import codecs
import hashlib
import html
import http.server
import json
import os
import re
import shlex
import shutil
import urllib
from typing import Any, Callable
from urllib.parse import urlparse, parse_qs
from urllib.request import urlopen, Request

from tim_common.cs_sanitizer import allow_minimal, tim_sanitize

CACHE_DIR = "/tmp/cache/"


class QueryClass:
    def __init__(self):
        self.get_query = {}
        self.query = {}
        self.jso = None
        self.deleted = {}
        self.randomcheck = ""
        self.cut_errors = []
        self.hide_program = False

    def set_jso_param(self, value, *keys):
        if self.jso is None:
            self.jso = {}
        q = self.jso
        for key in keys[:-1]:
            if key not in q:
                q[key] = {}
            q = q[key]
        q[keys[-1]] = value

    def set_param(self, value, *keys, field=None):  # , first_list=True):
        if len(keys) == 0:
            return

        if field is None:
            self.set_param(value, *keys, field=self.query)
            self.set_param(value, *keys, field=self.get_query)
            if self.jso is not None:
                self.set_jso_param(value, *keys)
            return

        q = field
        if len(keys) > 1:
            if keys[0] not in q:
                q[keys[0]] = [{}]
            q = q[keys[0]][0]
            for key in keys[1:-1]:
                if key not in q:
                    q[key] = {}
                q = q[key]
            q[keys[-1]] = value
        else:
            q[keys[-1]] = [value]


def check_key(query: QueryClass, key: str):
    if key in query.query:
        return key
    key2 = "-" + key
    if key2 in query.query:
        return key2
    if key in query.get_query:
        return key
    if key2 in query.get_query:
        return key2
    q_jso = query.jso
    if not q_jso:
        return key
    q_input = q_jso.get("input")
    if q_input:
        if key in q_input:
            return key
        if key2 in q_input:
            return key2
    m = q_jso.get("markup")
    if not m:
        return key
    if key in m:
        return key
    if key2 in m:
        return key2
    return key


def normalize_bool(value):
    if value in ("false", "False"):
        return False
    return value


def get_value(jso, default, *keys):
    d = jso
    for key in keys:
        if not d:
            return default
        if not isinstance(d, dict):
            return default
        d = d.get(key, None)
    if d is None:
        return default
    return d


def get_param(query: QueryClass, key, default):
    key = check_key(query, key)
    dvalue = default
    q_val = query.query.get(key)
    if q_val:
        dvalue = normalize_bool(q_val[0])
        if dvalue == "undefined":
            dvalue = default

    gq_val = query.get_query.get(key)
    if not gq_val:
        if query.jso is None:
            return dvalue
        q_input = query.jso.get("input")
        if q_input:
            value = q_input.get(key, "undefined")
            if value != "undefined":
                return value
        m = query.jso.get("markup")
        if not m:
            return dvalue
        return m.get(key, dvalue)
    value = gq_val[0]
    if value == "undefined":
        return dvalue
    return normalize_bool(value)


def get_param_table(query: QueryClass, key: str):
    value = get_param(query, key, None)
    if not value:
        return None
    if not isinstance(value, list):
        # or (len(number_rule) >= 1 and isinstance(number_rule[0], float)):
        # number_rule = [number_rule]
        return [value]
    return value


def get_2_items(js, key1: str, key2: str, def1=None, def2=None):
    if not isinstance(js, dict):
        return js, def2
    value1 = js.get(key1, def1)
    value2 = js.get(key2, def2)
    return value1, value2


def get_param_del(query: QueryClass, key: str, default: Any):
    key = check_key(query, key)
    if key not in query.query:
        if query.jso is None:
            return default
        if "markup" not in query.jso:
            return default
        if key in query.jso["markup"]:
            value = query.jso["markup"][key]
            del query.jso["markup"][key]
            query.deleted[key] = value
            return value
        return default
    value = query.query[key][0]
    del query.query[key]
    if value == "undefined":
        return default
    query.deleted[key] = value
    return value


def do_matcher(key: str):
    if not key:
        return False
    return re.compile(key)


def check(matcher, line: str):
    if not matcher:
        return False
    match = matcher.search(line)
    return match


def get_json_eparam(
    jso: dict[str, Any],
    key1: str,
    key2: str,
    default: Any,
    escape_html_special_chars: bool = True,
):
    # escaped param
    result = get_json_param(jso, key1, key2, default)
    if result is None:
        return None
    if not isinstance(result, str):
        # print("Ei ollut string: ", result, jso)
        result = "" + str(result)
    if escape_html_special_chars:
        return html.escape(result)
    return html.unescape(result)


def get_json_param(jso: dict[str, Any], key1: str, key2: str | None, default: Any):
    # noinspection PyBroadException
    try:
        if jso is None:
            return default
        if key1 not in jso:
            return default
        if not key2:
            return jso[key1]
        if not jso[key1]:
            return default
        if key2 not in jso[key1]:
            return default
        return jso[key1][key2]
    except:
        # print("JSO XXXXXXXXXXXXX", jso)
        print("KEY1=", key1, "KEY2=", key2)
        return default


def get_scan_value(s: str):
    direction = 1
    if s:
        if s[0] == "-":
            direction = -1
            s = s[1:]
        if s[0] == "+":
            s = s[1:]
    match = do_matcher(s.replace("\\\\", "\\"))
    return direction, match


def scan_lines(lines: list[str], n: int, i: int, scanner, direction: int):
    if not scanner:
        return i
    i += direction
    while 0 <= i < n:
        line = lines[i]
        if check(scanner, line):
            return i
        i += direction
    if i < 0:
        i = 0
    if i >= n:
        i = n - 1
    return i


cache = {}


def clear_cache():
    global cache
    cache = {}
    for f in os.listdir(CACHE_DIR):
        os.remove(CACHE_DIR + f)


def get_cache_keys():
    global cache
    s = ""
    memory_cache_size = len(cache)
    for key in cache.keys():
        s += key + "\n"
    disk_cache_size = 0
    # noinspection PyBroadException
    try:
        cache_dir_files = os.listdir(CACHE_DIR)
        disk_cache_size = len(cache_dir_files)
        s = s + "\n".join(cache_dir_files)
    except:  # no CACHE_DIR yet
        pass
    return s, memory_cache_size, disk_cache_size


# noinspection PyBroadException
def get_url_lines(url: str):
    # TODO: there might be a problem with sama cache for lines and
    # strings. If the same url is used for both, the cache will
    # return the wrong one. Maybe must use different keys for
    # lines (maybe add some LINE_ to the key saving lines)
    # And should be studied if cache is better with redis.
    # Now using memory cache, the speed difference is at least 10x
    # in small multihtml-route compared to empty cache.
    #
    global cache
    # print("========= CACHE KEYS ==========\n", get_cache_keys())
    if url in cache:
        # print("from cache: ", url)
        return cache[url]

    diskcache = CACHE_DIR + url.replace("/", "_").replace(":", "_")

    print("not in cache: ", url)
    # if False and os.path.isfile(diskcache):
    if os.path.isfile(diskcache):
        try:
            result = open(diskcache, encoding="iso8859_15").read()
            result = result.split("\n")
            print("from DISK cache: ", diskcache)
            cache[url] = result

            return result
        except Exception as e:
            print(str(e))
            print("error reading filecache: ", diskcache)

    check_url_scheme(url)
    try:
        chrome_req = urllib.request.Request(url, headers={"User-Agent": "Chrome"})
        req = urllib.request.urlopen(chrome_req)

        # ftype = req.headers['content-type']
        lines = req.readlines()
    except:
        return False
    # filecontent = nltk.clean_html(html)

    n = len(lines)

    for i in range(0, n):
        line = lines[i]
        try:
            line = line.decode("utf-8-sig")
        except:
            try:
                line = line.decode(encoding="iso8859_15")
            except:
                line = str(line)
        lines[i] = line.replace("\n", "").replace("\r", "")

    cache[url] = lines

    try:
        # open(diskcache,"w").write("\n".join(lines))
        if not os.path.isdir(CACHE_DIR):
            os.mkdir(CACHE_DIR)

        open(diskcache, "w", encoding="iso8859_15").write("\n".join(lines))
    except Exception as e:
        print(str(e))
        print("XXXXXXXXXXXXXXXXXXXXXXXX Could no write cache: \n", diskcache)

    return lines


def check_url_scheme(url: str):
    scheme = urlparse(url).scheme
    if scheme not in ("http", "https"):
        raise Exception(f"URL scheme must be http or https, got '{scheme}'")


def clean_url(url: str):
    i = url.find("?")
    if i < 0:
        return url
    return url[:i]


# noinspection PyBroadException
def get_url_lines_as_string(
    url: str, headers: dict[str, str] | None = None, not_found_error: str | None = None
):
    global cache
    cachename = "lines_" + url + secure_hash_dict(headers)
    diskcache = CACHE_DIR + cachename.replace("/", "_").replace(":", "_")
    # print("========= CACHE KEYS ==========\n", get_cache_keys())
    # print(cachename + "\n")
    # print(cache) # cache does not work in forkingMix
    if cachename in cache:
        # print("from cache: ", cachename)
        return cache[cachename]

    # print("not in cache: ", cachename)
    # return "File not found: " + url

    if os.path.isfile(diskcache):
        try:
            result = open(diskcache).read()
            cache[cachename] = result
            return result
        except:
            print("error reading filecache: ", diskcache)

    # print("not in filecache: ", diskcache)

    check_url_scheme(url)
    try:
        req = Request(url, headers=headers or {})
        res = urlopen(req)
        # ftype = req.headers['content-type']
        lines = res.readlines()
    except:
        if not not_found_error:
            return "File not found: " + clean_url(url)
        return not_found_error
    # filecontent = nltk.clean_html(html)

    n = len(lines)
    result = ""

    for i in range(0, n):
        line = lines[i]
        try:
            line = line.decode("utf-8-sig")
        except:
            try:
                line = line.decode(encoding="iso8859_15")
            except:
                line = str(line)
        result += line.replace("\r", "")

    result = result.strip("\n")
    cache[cachename] = result

    try:
        open(diskcache, "w").write(result)
    except:
        print("Could no write cache: ", diskcache)

    # print(cache)
    return result


def replace_random(query: QueryClass, s):
    if not hasattr(query, "randomcheck"):
        return s
    result = s
    # noinspection PyBroadException
    try:
        result = result.replace("RANDOMCHECK", query.randomcheck)
    except:
        pass
    return result


def replace_program_tokens(query: QueryClass, s: str) -> str:
    replace_tokens = get_param(query, "replaceProgramTokens", None)
    if replace_tokens is None or not isinstance(replace_tokens, dict):
        return s
    for token, value in replace_tokens.items():
        if value is None:
            value = ""
        s = s.replace(token, str(value))
    return s


def do_escape(s: str):
    line = html.escape(s)
    # line = line.replace("{","&#123;")
    #  because otherwise problems with angular {{, no need if used inside ng-non-bindable
    # line = line.replace("}","&#125;") # because otherwise problems with angular }}
    return line


class FileParams:
    def __init__(self, query: QueryClass, nr: str, url: str):  # , **defs):
        self.url = get_param(query, "file" + nr, "")  # defs.get('file',""))
        self.start = do_matcher(
            get_param(query, "start" + nr, "").replace("\\\\", "\\")
        )
        self.start_scan_dir, self.start_scan = get_scan_value(
            get_param(query, "startscan" + nr, "")
        )
        self.startcnt = int(get_param(query, "startcnt" + nr, "1"))
        self.startn = int(get_param(query, "startn" + nr, "0"))
        self.end = do_matcher(get_param(query, "end" + nr, "").replace("\\\\", "\\"))
        self.end_scan_dir, self.end_scan = get_scan_value(
            get_param(query, "endscan" + nr, "")
        )
        self.endcnt = int(get_param(query, "endcnt" + nr, "1"))
        self.endn = int(get_param(query, "endn" + nr, "0"))
        self.linefmt = get_param(query, "linefmt" + nr, "")
        self.maxn = int(get_param(query, "maxn" + nr, "10000"))
        self.lastn = int(get_param(query, "lastn" + nr, "1000000"))
        self.include: str = get_param(query, "include" + nr, "")
        self.replace = do_matcher(get_param(query, "replace" + nr, ""))
        self.by = replace_random(query, get_param(query, "by" + nr, ""))
        if not self.by and nr == "":
            self.by = replace_random(query, get_param(query, "byCode" + nr, ""))
        self.prorgam = replace_random(query, get_param(query, "program" + nr, ""))

        self.reps = []

        rep_nr = ""
        if nr:
            rep_nr = nr + "."

        for i in range(1, 10):
            rep = do_matcher(
                get_param(query, "replace" + rep_nr + str(i), "")
            )  # replace.1.1 tyyliin replace1 on siis replace.0.1
            if not rep:
                break
            byc = replace_random(
                query, get_param(query, "byCode" + rep_nr + str(i), "")
            )
            self.reps.append({"by": rep, "bc": byc})

        usercode = get_json_param(query.jso, "input" + nr, "usercode", None)
        # if ( query.jso != None and query.jso.has_key("input") and query.jso["input"].has_key("usercode") ):
        if isinstance(usercode, str):
            self.by = usercode
        else:
            usercode = get_json_param(query.jso, "state" + nr, "usercode", None)
            if isinstance(usercode, str):
                self.by = usercode

        u = get_param(query, "url" + nr, "")
        if u and not self.url:
            self.url = url
        # if self.url: print("url: " + self.url + " " + self.linefmt + "\n")

    def get_file(self, escape_html=False, not_found_error=None):
        if self.prorgam:
            # print(self.prorgam)
            return self.scan_needed_lines(self.prorgam.split("\n"), escape_html)
        if not self.url:
            # print("SELF.BY:", self.by.encode())
            if not self.by:
                return ""
            return self.by  # self.by.replace("\\n", "\n")

        lines = get_url_lines(self.url)
        if not lines:
            if not_found_error:
                return not_found_error
            return "File not found " + clean_url(self.url)

        return self.scan_needed_lines(lines, escape_html)

    def scan_needed_range(self, lines):
        n = len(lines)
        n1 = scan_lines(lines, n, 0, self.start, 1)
        n2 = n1
        n1 = scan_lines(lines, n, n1, self.start_scan, self.start_scan_dir)
        n1 += self.startn
        if n1 < 0:
            n1 = 0
        if n2 < n1:
            n2 = n1  # if n1 went forward

        if self.end:
            n2 = scan_lines(lines, n, n2, self.end, 1)
            n2 = scan_lines(lines, n, n2, self.end_scan, self.end_scan_dir)
        else:
            n2 = n - 1
        n2 += self.endn
        if n2 >= n:
            n2 = n - 1
        return n1, n2

    def scan_needed_lines(self, lines, escape_html=False):
        (n1, n2) = self.scan_needed_range(lines)
        ni = 0

        result = ""

        replace_by = self.by
        if replace_by:
            rep = replace_by.split("\n")
            if (
                len(rep) > 0 and rep[0].strip() == "//"
            ):  # remove empty comment on first line (due YAML limatations)
                del rep[0]
                replace_by = "\n".join(rep)

        for i in range(n1, n2 + 1):
            line = lines[i]
            # if enc or True: line = line.decode('UTF8')
            # else: line = str(line)
            if check(self.replace, line):
                line = replace_by  # + "\n"
            for r in self.reps:
                if check(r["by"], line):
                    line = r["bc"]  # + "\n"

            if escape_html:
                line = html.escape(line)
            ln = self.linefmt.format(i + 1)
            result += ln + line + "\n"
            if i + 1 >= self.lastn:
                break
            ni += 1
            if ni >= self.maxn:
                break

        return result

    def get_include(self, escape_html=False):
        if not self.include:
            return ""
        data = self.include.replace("\\n", "\n")
        if escape_html:
            data = html.escape(data)
        return data


def get_params(self):
    result = QueryClass()
    result.get_query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    return result


def get_file_to_output(query: QueryClass, show_html: bool, p0: FileParams = None):
    try:
        not_found_error = query.jso.get("markup", {}).get("notFoundError", None)
        if p0 is None:
            p0 = FileParams(query, "", "")
        # if p0.url == "":
        s = p0.get_file(show_html, not_found_error=not_found_error)
        # if not s:
        # return "Must give file= -parameter"
        s += p0.get_include(show_html)
        u = p0.url
        for i in range(1, 10):
            p = FileParams(query, "." + str(i), u)
            s += p.get_file(show_html, not_found_error=not_found_error)
            s += p.get_include(show_html)
            if p.url:
                u = p.url
        return s
    except Exception as e:
        return str(e)


number_of_multi_html_reqs = 0


def multi_post_params(self) -> list[QueryClass]:
    content_length = int(self.headers["Content-Length"])
    f = self.rfile.read(content_length)
    u = f.decode("UTF8")
    jsos = json.loads(u)
    results = []
    for jso in jsos:
        results.append(get_query_from_json(jso))
    return results


def get_query_from_json(jso) -> QueryClass:
    result = QueryClass()
    result.jso = jso
    for field, value in result.jso.items():
        if field not in ("state", "input"):
            result.query[field] = [value]
    return result


def post_params(self: http.server.BaseHTTPRequestHandler) -> QueryClass:
    # print "postParams ================================================"
    # print self
    # pprint(self.__dict__,indent=2)
    # print dir(self.request)
    # print(self.path)
    # print(self.headers)
    content_length = int(self.headers["Content-Length"])
    content_type = "application/json"
    if "Content-Type" in self.headers:
        content_type = self.headers["Content-Type"]
    if "content-cype" in self.headers:
        content_type = self.headers["content-type"]

    f = self.rfile.read(content_length)
    # print(f)
    # print(type(f))
    u = f.decode("UTF8")
    # print(u)
    # print(type(u))

    result = QueryClass()
    result.query = {}  # parse_qs(urlparse(self.path).query, keep_blank_values=True)
    # print("result.query ================================== ")
    # pp = pprint.PrettyPrinter(indent=4)
    # pp.pprint(result.query)

    if len(u) == 0:
        return result
    if content_type.find("json") < 0:  # ei JSON
        # print("POSTPARAMS============")
        q = parse_qs(urlparse("k/?" + u).query, keep_blank_values=True)
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


def file_to_string(name: str):
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    result = ""
    for i in range(0, len(lines)):
        line = lines[i]
        result += line
    fr.close()
    return result


def query_params_to_angular(query: dict[str, Any]):
    result = ""
    for field in query.keys():
        result = result + field + '="' + query[field][0] + '";\n'
    # print "QUERY" + str(query)
    return result


def query_params_to_attribute(query, leave_away):
    result = ""
    # print("leave_away " + leave_away)
    for field in query.keys():
        if not (leave_away and field == leave_away):
            result = result + field.lower() + "='" + query[field][0] + "'\n"
    # print "QUERY" + str(query)
    return result + ""


def default_value_transform(x):
    return x[0]


def query_params_to_map(
    query: dict[str, Any], transform=None, deny: dict[str, Any] = None
):
    if transform is None:
        transform = default_value_transform
    result = {}
    for field, value in query.items():
        if field.startswith("-"):
            continue
        if deny and field in deny:
            continue
        result[field] = transform(value)
    m = result.get("markup")
    if m:
        result["markup"] = query_params_to_map(m, transform=lambda x: x, deny=deny)
    return result


def query_params_to_map_check_parts(query: QueryClass, deny: dict[str, Any] = None):
    result = query_params_to_map(query.query, deny=deny)
    return result


def string_to_string_replace_url(line: str, what_to_replace: str, query: QueryClass):
    qmap = query_params_to_map(query.query)
    params = urllib.parse.urlencode(qmap)
    line = line.replace(what_to_replace, params)
    height = get_param(query, "height", "100%")
    line = line.replace("##HEIGHT##", str(height))
    return line


def string_to_string_replace_attribute(
    line: str, what_to_replace: str, query: QueryClass
):
    leave_away = None
    if "##USERCODE##" in line:
        leave_away = "byCode"
    params = query_params_to_attribute(query.query, leave_away)
    line = line.replace(what_to_replace, params)
    by = get_param(query, "byCode", "")
    by = get_param(query, "usercode", by)
    # print("BY XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", by.encode())
    line = line.replace("##USERCODE##", by)
    # print(line.encode())
    return line


def clean(s):
    s = str(s)
    s = s.replace('"', "")  # kannattaako, tällä poistetaan katkaisun mahdollisuus?
    return s
    # return bleach.clean(s)


attrs = {"*": ["id", "class"], "a": ["href"]}
tags = [
    "a",
    "p",
    "em",
    "strong",
    "tt",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "i",
    "b",
    "code",
]


def get_heading(query: QueryClass, key: str, def_elem: str, el_attrs: str = ""):
    if not query:
        return ""
    # return "kana"
    h = get_param(query, key, None)
    # print("h=",h)
    if not h:
        return ""
    h = tim_sanitize(str(h))
    if el_attrs:
        el_attrs = f" {el_attrs}"
    return "<" + def_elem + el_attrs + ">" + h + "</" + def_elem + ">\n"


def get_md_heading(query: QueryClass, key: str, def_elem: str):
    if not query:
        return ""
    # return "kana"
    h = get_param(query, key, None)
    # print("h=",h)
    if not h:
        return ""
    # h = tim_sanitize(str(h)) # TODO: how to sanitaze for md?
    h = str(h)
    if not def_elem:
        return h
    return "\\" + def_elem + "{" + h + "}\n"

    # Loppu on liian hidasta koodia, kannattaa muuten vaihtaa järjestys jos tuota vielä käyttää, samoin js-koodissa
    #  st = h.split("!!")  # h4 class="h3" width="23"!!Tehtava 1
    #  elem = def_elem
    #  val = st[0]
    #  attributes = ""
    #  if len(st) >= 2:
    #      elem = st[0]
    #      val = st[1]
    #  i = elem.find(' ')
    #  ea = [elem]
    #  if i >= 0:
    #      ea = [elem[0:i], elem[i:]]
    #  if len(ea) > 1:
    #      elem = ea[0]
    #      attributes = ea[1] + " "
    #  val = allow(val)
    #  result_html = "<" + elem + attributes + ">" + val + "</" + elem + ">\n"
    #  # result_html = bleach.clean(result_html, tags, attrs)
    #  return result_html


def get_surrounding_headers2(query: QueryClass):
    result = get_heading(query, "header", "h4")
    stem = allow_minimal(get_param(query, "stem", None))
    if stem:
        result += '<p class="stem" >' + stem + "</p>"
    return result, get_heading(query, "footer", "p", 'class="plgfooter"')


def get_surrounding_md_headers2(query: QueryClass, header_style, footer_style):
    result = get_md_heading(query, "header", header_style)
    stem = allow_minimal(get_param(query, "stem", None))
    if stem:
        result += "\n\n" + stem
    return result, get_md_heading(query, "footer", footer_style)


def get_tiny_surrounding_headers(query: QueryClass, inside):
    result = get_heading(query, "header", "h4")
    header = get_param(query, "header", None)
    stem = tim_sanitize(get_param(query, "stem", None))
    if not stem and not header and not inside:
        stem = "Open plugin"
    if stem:
        result += '<span class="stem" >' + stem + "</span>"
    result += '<span class="csTinyText" >' + inside + "</span>\n"
    return result


def get_surrounding_headers(query: QueryClass, inside):
    result = get_heading(query, "header", "h4")
    stem = tim_sanitize(get_param(query, "stem", None))
    if stem:
        result += '<p class="stem" >' + stem + "</p>\n"
    result += inside + "\n"
    result += get_heading(query, "footer", 'p class="plgfooter"')
    return result


def get_surrounding_md_headers(query: QueryClass, inside, extra):
    result = get_md_heading(query, "header", "pluginHeader")
    stem = tim_sanitize(get_param(query, "stem", None))
    if stem:
        result += "\n\n" + stem
    result += "\n``````\n" + inside + "\n``````\n"
    result += extra
    result += get_md_heading(query, "footer", "plgfooter")
    return result


def get_clean_param(query: QueryClass, key, default):
    s = get_param(query, key, default)
    # return str(s)
    return clean(s)


def do_headers(self, content_type):
    self.send_response(200)
    self.send_header("Access-Control-Allow-Origin", "*")
    self.send_header("Access-Control-Allow-Methods", "GET, PUT, POST, OPTIONS")
    self.send_header(
        "Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type"
    )
    self.send_header("Content-type", content_type)
    self.end_headers()


# Etsii paketin ja luokan nimen tiedostosta
def find_java_package(s):
    p = ""
    c = ""
    r = re.search(r"package\s*([\s.a-zA-Z0-9_]+)", s, flags=re.M)
    if r:
        p = re.sub(r"\s*", "", r.group(1))
    r = re.search(r"public\s*class\s*([a-zA-Z0-9_]+)", s, flags=re.M)
    if r:
        c = r.group(1)
    else:
        r = re.search(r"public\s*interface\s*([a-zA-Z0-9_]+)", s, flags=re.M)
        if r:
            c = r.group(1)

    return p, c


# Etsii C# luokan nimen tiedostosta
def find_cs_class(s):
    # TODO: better check if class is inside namespace
    c = "Peli"
    r = re.search(r"public\s*class\s*([a-zA-Z0-9_]+)", s, flags=re.M)
    if r:
        c = r.group(1)
    r = re.search(r"namespace\s*([a-zA-Z0-9_]+)", s, flags=re.M)
    if r:
        c = r.group(1) + "." + c

    return c


# Palauttaa intin joka löytyy jonon alusta
def getint(s):
    i = 0
    s = s.strip(" ")
    while i < len(s):
        if "0123456789".find(s[i : i + 1]) < 0:
            if i == 0:
                return 0
            return int(s[0:i])
        i += 1
    return int(s)


def secure_hash_dict(d: dict[str, str] | None) -> str:
    if not d:
        return ""
    dk = hashlib.pbkdf2_hmac("sha256", str.encode(str(d)), b"timdict", 100)
    return dk.hex()


# see: https://docs.python.org/3/library/hashlib.html
def hash_user_dir(user_id):
    dk = hashlib.pbkdf2_hmac("sha256", str.encode(user_id), b"tim", 100)
    return bytes.decode(binascii.hexlify(dk))


# Korvataan sisällössä scriptit
def replace_scripts(s, scripts, placeholder):
    sc = ""
    if scripts != "":
        scs = scripts.split(",")
        for s1 in scs:
            sc += '<script src="' + s1 + '"></script>\n'
    return s.replace(placeholder, sc)


def get_templates(dirname: str) -> object:
    """Find all templates from dirname.  Each template file should include.

    template text - first line
    template explanation - second line
    template content - rest of lines

    :param dirname: the directory name where to find template files
    :return: list of template items, one item is file: text: explanation
    """
    result = []
    for filename in os.listdir(dirname):
        f = open(dirname + "/" + filename, encoding="utf-8-sig").readlines()
        template = {"file": filename, "text": f[0].strip(), "expl": f[1].strip()}
        result.append(template)

    return result


def get_all_templates(dirname: str) -> dict:
    """Find list of all templates from dirname.  Dir should include file tabs.txt where there is one line for every tab
    needed for TIM editor. Then there should be directories 0, 1, ... for each corresponding tab-line.  So if there is
    two lines in tabs.txt there is directories 0 and 1 for first and second tab.

    :param dirname: the directory name where to find template list file and sub directories for templates
    :return: dict with list of lif to template items (templates) and texts (text)

    """
    templates = []
    try:
        texts = open(dirname + "/tabs.txt", encoding="utf-8-sig").read().splitlines()
        for i in range(0, len(texts)):
            templates.append(get_templates(dirname + "/" + str(i)))
    except Exception as e:
        print(e)
        return {}
    return {"templates": templates, "text": texts}


def get_template(dirname: str, idx: str, filename: str) -> str:
    """Returns the template file from line 2 to end of file.

    :param dirname: from directory (be sure this is valid)
    :param idx: index for the template  (only numbers allowed)
    :param filename: from file (validity of this is checked)
    :return: lines starting from line 2.

    """
    try:
        fname = re.sub(r"[^ A-ZÅÄÖa-zåäö_0-9]", "", filename)
        if not fname:
            fname = "0"
        tidx = re.sub(r"[^0-9]", "", idx)
        f = open(dirname + "/" + tidx + "/" + fname, encoding="utf-8-sig").readlines()
    except Exception as e:
        return str(e)
    return "".join(f[2:])


def join_dict(a: dict, b: dict):
    """Joins two dict and returns a new one.

    :rtype: dict
    :param a: first dict to join
    :param b: next dict to join
    :return: "a+b"

    """
    result = a.copy()
    result.update(b)
    return result


LAZYSTART = "<!--lazy "
LAZYEND = " lazy-->"
NOLAZY = "<!--nolazy-->"
NEVERLAZY = "NEVERLAZY"


def is_lazy(query: QueryClass):
    # print(query)
    caller_lazy = get_param(query, "doLazy", NEVERLAZY)
    # print("caller_lazy=",caller_lazy)
    if caller_lazy == NEVERLAZY:
        return False
    do_lazy = caller_lazy
    if str(do_lazy).lower() == "true":
        do_lazy = True
    if str(do_lazy).lower() == "false":
        do_lazy = False
    lazy = get_param(query, "lazy", "")
    if str(lazy).lower() == "true":
        do_lazy = True
    if str(lazy).lower() == "false":
        do_lazy = False
    # print("do_lazy=",do_lazy)
    return do_lazy


def is_user_lazy(query: QueryClass):
    caller_lazy = get_param(query, "doLazy", NEVERLAZY)
    # print("caller_lazy=",caller_lazy)
    if caller_lazy == NEVERLAZY:
        return False
    do_lazy = False
    if str(caller_lazy).lower() == "false":
        return False
    lazy = get_param(query, "lazy", "")
    if str(lazy).lower() == "true":
        do_lazy = True
    # print("do_lazy=",do_lazy)
    return do_lazy


def add_lazy(plugin_html: str) -> str:
    return LAZYSTART + plugin_html + LAZYEND


def make_lazy(
    plugin_html: str, query: QueryClass, htmlfunc: Callable[[QueryClass], str]
) -> str:
    """Makes plugin string to lazy.

    :param plugin_html: ready html for the plugin
    :param query: query params where lazy options can be read
    :param htmlfunc: function to generate the lazy version of plugin html
    :return true if lazy plugin is needed

    """
    if not is_lazy(query):
        return plugin_html
    lazy_html = tim_sanitize(htmlfunc(query))
    lazy_plugin_html = LAZYSTART + plugin_html + LAZYEND + lazy_html
    return lazy_plugin_html


def replace_template_params(
    query: QueryClass, template: str, cond_itemname: str, itemnames=None
) -> str:
    """Replaces all occurrences of itemnames and cond_item_name in template by their value in query if  cond_itemname
    exists in query.

    :param query: query params where items can be read
    :param template: string that may include items like {{userword}} that are replaced
    :param cond_itemname: name for the item that decides if the template is non empty.  None means no condition
    :param itemnames: list of other item names that are replaced by their value in Query
    :return true if lazy plugin is needed

    """
    items = []
    if cond_itemname:
        item = get_param(query, cond_itemname, "")
        if not item:
            return ""
        items = [cond_itemname]

    if itemnames:
        items += itemnames
    result = template

    for name in items:
        n, d, dummy = (name + "::").split(":", 2)
        item = str(get_param(query, n, d))
        result = result.replace("{{" + n + "}}", item)

    return result


def replace_template_param(
    query: QueryClass, template: str, cond_itemname: str, default=""
) -> str:
    """Replaces all occurrences of itemnames and cond_item_name in template by their value in query if  cond_itemname
    exists in query.

    :param query: query params where items can be read
    :param template: string that may include items like {{userword}} that are replaced
    :param cond_itemname: name for the item that decides if the template is non empty.  None means no condition
    :param default: value to be used if item is default or no item at all, if defult=="", return "" if used
    :return replaced template or ""

    """
    if not cond_itemname:
        return ""
    item = get_param(query, cond_itemname, "default")
    if item == "default":
        item = default
    if item == "":
        return ""
    if not item:
        return ""

    result = template.replace("{{" + cond_itemname + "}}", str(item))

    return result


def is_review(query: QueryClass):
    # print(query)
    result = get_param(query, "review", False)
    return result


def mkdirs(path: str):
    os.makedirs(path, exist_ok=True)
    shutil.chown(path, user="agent", group="agent")


def remove(fname: str):
    # noinspection PyBroadException
    try:
        os.remove(fname)
    except:
        return


def remove_before(what: str, s: str):
    # print("=================================== WHAT ==============")
    # print(what, " ", s)
    i = s.find(what)
    if i < 0:
        return s
    s = s[i + 1 :]
    i = s.find("\n")
    if i < 0:
        return ""
    return s[i + 1 :]


def tquote(s: str):
    if s.startswith("$"):
        return s
    r = shlex.quote(s)
    if r.find("$") < 0:
        return r
    return r.replace("'", '"')


def str_to_int(s: str, default=0):
    try:
        return int(s)
    except ValueError:
        return default


def encode_json_data(d: str):
    return base64.b64encode(d.encode()).decode()
