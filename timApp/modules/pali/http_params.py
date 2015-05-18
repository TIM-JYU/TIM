# -*- coding:utf-8 -*-

__author__ = 'vesal'

import http.server
import json
import codecs
from urllib.parse import urlparse, parse_qs


class QueryParams:
    def __init__(self, jso: dict=None):
        self.__get_query = {}
        self.__jso = jso
        if not self.__jso: self.__jso = {}

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
        Check if either "key" ot "-key" exists and return the one that works
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

    def get_param(self, key: str, default: object) -> object:
        """
        Get param value from query params that match to key.  If no match, then return default
        input is prefered over markup
        :param self: query params where to find the key
        :param key: key to find
        :param default: value to return if key not found
        :return:
        """
        key = self.__check_key(key)
        dvalue = default
        if dvalue == 'undefined':
            dvalue = default

        if key not in self.__get_query:
            if self.__jso is None: return dvalue
            if "input" in self.__jso and "markup" in self.__jso["input"] and key in self.__jso["input"]["markup"]:
                value = self.__jso["input"]["markup"][key]
                if value != 'undefined': return value

            if "markup" not in self.__jso: return dvalue
            if key in self.__jso["markup"]: return self.__jso["markup"][key]
            return dvalue
        value = self.__get_query[key][0]
        if value == 'undefined': return dvalue
        return value

    def copy_post_params(self, q: dict):
        """
        copy get params from q to jso
        :param q: get apras as dictionary
        """
        for field in list(q.keys()):
            self.__jso[field] = q[field][0]

    def set_get_params(self, get_p: dict):
        self.__get_query = get_p

    def get_json_param(self, key1: str, key2: str, default):
        jso = self.__jso
        try:
            if jso is None: return default
            if key1 not in jso: return default
            if not key2: return jso[key1]
            if not jso[key1]: return default
            if key2 not in jso[key1]: return default
            return jso[key1][key2]
        except Exception as e:
            # print("JSO XXXXXXXXXXXXX", jso)
            print(e, "KEY1=", key1, "KEY2=", key2)
            return default


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
    Copies recursive all fields form jso to result if field matches to f
    :param jso: params to copy
    :param f: function to return true if param should be copied
    :return: new dict with all fileds matching to predicate f
    """
    result = {}
    keys = jso.keys()
    for field in keys:
        if f(field):
            if isinstance(jso[field], dict):
                result[field] = copy_jso(jso[field], f)
            else:
                result[field] = jso[field]
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
    print(f)
    u = f.decode("UTF8")

    result = QueryParams()

    if len(u) == 0: return result
    if content_type.find("json") < 0:  # ei JSON
        q = parse_qs(urlparse('k/?' + u).query, keep_blank_values=True)
        result.copy_post_params(q)
    else:
        jso = json.loads(u)
        result = QueryParams(jso)

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
    # hihkse, kun jatkat ;-)


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
