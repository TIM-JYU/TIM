# -*- coding:utf-8 -*-

__author__ = 'vesal'

import http.server
import json
import codecs
import bleach
import os
import re
from urllib.parse import urlparse, parse_qs

def check_value(value: object, default: object) -> object:
    """
    Check if value is something real, if not return default
    :param value: value to check
    :param default: value to return if value is not good
    :return: value or default
    """
    if value == 'undefined': return default
    if value is None: return default
    return value


class QueryParams:
    """
    Class for holding GET params and possible POST params as a json-object
    """
    def __init__(self, jso: dict=None):
        """
        Luodaan QueryParams olio
        :rtype : QueryParams
        """
        self.__get_query = {}
        self.__jso = jso
        if not self.__jso: self.__jso = {}

    def __repr__(self) -> str:
        """
        :return: QueryParams as string
        """
        return str(self.__get_query) + " " + str(self.__jso)

    def to_json(self, f) -> dict:
        """
        Returns json object from all params without keys passing predicate f
        :param self: get params
        :param f: function to return true if param is copied
        :return: params as json object
        """
        result = copy_jso(self.__jso, f)
        return result

    def __check_key(self, key: str) -> str:
        """
        Check if either "key" or "-key" exists and return the one that works
        :param self: query parmas where to find
        :param key: key to find
        :return: key or -key
        """
        key2 = "-" + key
        if key in self.__get_query: return key
        if key2 in self.__get_query: return key2
        if not self.__jso: return key
        if "input" in self.__jso and "markup" in self.__jso["input"]:
            if key in self.__jso["input"]["markup"]: return key
            if key2 in self.__jso["input"]["markup"]: return key2
        if "markup" not in self.__jso: return key
        if key in self.__jso["markup"]: return key
        if key2 in self.__jso["markup"]: return key2
        return key


    def get_param_from_json(self, key: str, default: object) ->  object: # return shoud be Any ???
        """
        Get param value from query's json part.  key may include . so that "state.userword" gives a
        value from inside "state" with key "userword".  If no match, then return default
        input is prefered over markup
        :param self: query params where to find the key
        :param key: key to find, may contain . and if . is first, from higest level
        :param default: value to return if key not found
        :return: param from key or default
        """
        # print(self.dump(),"KEY:",key)
        keys = key.split(".")
        if keys[0] == "": keys = keys[1:]
        level = self.__jso
        if not level: return default
        for k in keys[:-1]:
            if k not in level: return default
            level = level[k]
            if not level: return default
        lk = keys[-1]
        if lk not in level: lk = "-"+lk
        if lk not in level: return default

        return level[lk]


    def get_param(self, key: str, default: object) ->  object: # return shoud be Any ???
        """
        Get param value from query params that match to key.  If no match, then return default
        input is prefered over markup
        :param self: query params where to find the key
        :param key: key to find
        :param default: value to return if key not found
        :return: param from key or default
        """
        if key.find(".") >= 0: return self.get_param_from_json(key, default)

        key = self.__check_key(key)
        dvalue = default
        if dvalue == 'undefined':
            dvalue = default

        if key not in self.__get_query:
            if self.__jso is None: return dvalue
            if "input" in self.__jso and "markup" in self.__jso["input"] and key in self.__jso["input"]["markup"]:
                value = self.__jso["input"]["markup"][key]
                return check_value(value,dvalue)

            if "markup" not in self.__jso: return dvalue
            if key in self.__jso["markup"]:
                value = self.__jso["markup"][key]
                return check_value(value,dvalue)

            if key in self.__jso: return self.__jso[key]
            return dvalue
        value = self.__get_query[key][0]
        return check_value(value,dvalue)

    def copy_post_params(self, q: dict):
        """
        copy get params from q to jso
        :param q: get apras as dictionary
        """
        for field in list(q.keys()):
            self.__jso[field] = q[field][0]

    def set_get_params(self, get_p: dict):
        """
        set new value for GET params
        :param get_p: new value to set
        """
        self.__get_query = get_p

    def get_json_param(self, key1: str, key2: str, default) -> object:
        """
            Get param only from json part and from key1.key2
            :param key1: first key
            :param key2: second key
            :param default: value if not found
            :return: param from key1.key2 or default
            """
        jso = self.__jso
        try:
            if jso is None: return default
            if key1 not in jso: return default
            if not key2: return jso[key1]
            if not jso[key1]: return default
            if key2 not in jso[key1]: return default
            if type(jso[key1]) is str:
                jso1 = json.loads(jso[key1])
                if key2 not in jso1: return default
                return jso1[key2]
            return jso[key1][key2]
        except Exception as e:
            # print("JSO XXXXXXXXXXXXX", jso)
            print(e, "KEY1=", key1, "KEY2=", key2)
            return default

    def get_sanitized_param(self, key: str, default: str) -> str:
        """
        get totally sanitized parameter where all html tags are removed
        :param self: query params where to find the key
        :param key: key to find
        :param default: value to return if key not found
        :return: param from key or default
        """
        s = str(self.get_param(key, default))
        return sanitize_all(s)

    def dump(self):
        """
        dumps json part as a string
        """
        return json.dumps(self.__jso)

def do_headers(response: http.server.BaseHTTPRequestHandler, content_type: str):
    """
    Make headers for this response
    :param response: http_response to send
    :param content_type: content type for this response
    :return: nothing
    """
    response.send_response(200)
    response.send_header('Access-Control-Allow-Origin', '*')
    response.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
    response.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
    response.send_header('Content-type', content_type)
    response.end_headers()


def copy_jso(jso: dict, f) -> dict:
    """
    Copies recursive all fields form jso to result if field matches to predicate f(key)
    :param jso: params to copy
    :param f: function to return true if param should be copied
    :return: new dict with all fileds matching to predicate f
    """
    result = {}
    keys = jso.keys()
    for key in keys:
        if f(key):
            if isinstance(jso[key], dict):
                result[key] = copy_jso(jso[key], f)
            else:
                result[key] = jso[key]
    return result


def accept_nonhyphen(s: str) -> bool:
    """
    Return true if s does not start with hyphen
    :param s: string to check
    :return: true if s does not start with hyphen
    """
    return not s.startswith("-")


def get_params(request: http.server.BaseHTTPRequestHandler) -> QueryParams:
    """
    get GET params from the request
    :param request: request to parse
    :return: query params
    """
    result = QueryParams()
    result.set_get_params(parse_qs(urlparse(request.path).query, keep_blank_values=True))
    return result


def post_params(request: http.server.BaseHTTPRequestHandler) -> QueryParams:
    """
    get POST params and get params from the request
    post params might me "normal" or json object
    :param request: request to parse
    :return: query params
    """
    content_length = int(request.headers['Content-Length'])
    content_type = "application/json"
    if 'Content-Type' in request.headers: content_type = request.headers['Content-Type']
    if 'content-type' in request.headers: content_type = request.headers['content-type']

    f = request.rfile.read(content_length)
    # print(f)
    u = f.decode("UTF8")

    result = QueryParams()

    if len(u) == 0: return result
    if content_type.find("json") < 0:  # ei JSON
        q = parse_qs(urlparse('k/?' + u).query, keep_blank_values=True)
        result.copy_post_params(q)
    else:
        try:
            jso = json.loads(u)
            result = QueryParams(jso)
        except Exception as e:
            print(str(e))

    get_query = parse_qs(urlparse(request.path).query, keep_blank_values=True)
    result.set_get_params(get_query)

    return result


def multi_post_params(request: http.server.BaseHTTPRequestHandler) -> [QueryParams]:
    """
    get all POST params from the request, they must be and array of JSON params
    :param request: request to parse
    :return: query params
    :rtype:
    """
    content_length = int(request.headers['Content-Length'])
    f = request.rfile.read(content_length)
    u = f.decode("UTF8")
    jsons = json.loads(u)
    # joko:
    results = []
    for jso in jsons:
        results.append(QueryParams(jso))
    return results
    # tai:
    # return {QueryParams(jso) for jso in jsons]


def file_to_string(name: str) -> str:
    """
    Returns the fila as a string
    :param name: name of teh file
    :return: file contents as a string
    """
    fr = codecs.open(name, encoding="utf-8-sig")
    lines = fr.readlines()
    result = ""
    for i in range(0, len(lines)):
        line = lines[i]
        result += line
    fr.close()
    return result


def allow(s: str) -> str:
    """
    Return string where few tags are allowed. Others are escaped
    :param s: string to allow tags
    :return: string where allowed tags a on place.
    """
    tags = ['em', 'strong', 'tt', 'a', 'b', 'code', 'i', 'kbd']
    attrs = {
        'a': ['href']
    }
    return bleach.clean(s, tags, attrs)


def sanitize_all(s: str) -> str:
    """
    Return string with all html tags are removed
    :param s: string that may have tags
    :return: string without any tags
    """
    tags = []
    attrs = { }
    return bleach.clean(s, tags, attrs, strip=True)


def clean(s: str) -> str:
    """
    Return string with all html tags are escaped
    :param s: string that may have tags
    :return: string with all tags escaped tags
    """
    s = str(s)
    s = s.replace('"', '')  # kannattaako, tällä poistetaam katkaisun mahdollisuus?
    return bleach.clean(s)


def check_array(a: object, ny: int, nx: int) -> bool:
    """
    Check if a is an array of ny rows and ny colums
    :param a: object to check
    :param ny: number of needed rows
    :param nx: number of needed lines
    :return: true is proper array
    """
    if not isinstance(a,list): return False
    if len(a) != ny: return False
    for row in a:
        if not isinstance(row,list): return False
        if len(row) != nx: return False

    return True


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
        f = open(dirname+"/"+filename).readlines()
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
        texts = open(dirname+"/tabs.txt").read().splitlines();
        for i in range(0, len(texts)):
            templates.append(get_templates(dirname+"/"+str(i)))
    except Exception as e:
        return {}
    return {'templates': templates, 'text': texts}


def get_template(dirname: str, idx: str, filename: str) -> str:
    """
    Returns the template file from line 2 to end of file
    :param dirname: from directory (be sure this is valid)
    :param idx: index for the template  (only numbers allowed)
    :param filename: from file (validity of this is checked)
    :return: lines starting from line 2.
    """
    try:
        fname = re.sub(r"[^ A-ZÅÄÖa-zåäö_0-9]","",filename)
        tidx = re.sub(r"[^0-9]","",idx)
        f = open(dirname+"/"+tidx+"/"+fname).readlines()
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
 
 
def is_lazy(query: QueryParams) -> bool:
    """
    Tells if plugins need to be done in lazy-mode
    :param query: query params where lazy options can be read 
    :return true if lazy plugin is needed
    """
    caller_lazy = query.get_param("doLazy", NEVERLAZY)
    # print("caller_lazy=",caller_lazy)
    if caller_lazy == NEVERLAZY: return False
    do_lazy = caller_lazy
    if str(do_lazy).lower() == "true":  do_lazy = True
    if str(do_lazy).lower() == "false": do_lazy = False
    lazy = query.get_param("lazy", "")
    if str(lazy).lower() == "true":  do_lazy = True
    if str(lazy).lower() == "false": do_lazy = False
    # print("do_lazy=",do_lazy)
    return do_lazy  
        
    
def make_lazy(plugin_html: str, query: QueryParams, htmlfunc) -> str:
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
    
    
def replace_template_params(query: QueryParams, template: str, cond_itemname: str, itemnames=None) -> str:
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
        item = query.get_param(cond_itemname, "")
        if not item: return ""
        items = [cond_itemname]
     
    if itemnames: items += itemnames
    result = template
       
    for name in items:
        n,d,dummy = (name+"::").split(":",2)
        item = str(query.get_param(n, d))
        result = result.replace("{{"+n+"}}", item)
        
    return result    
    