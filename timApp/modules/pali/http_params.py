# -*- coding:utf-8 -*-

__author__ = 'vesal'

import json
import codecs
from urllib.parse import urlparse, parse_qs

class QueryClass:
    def __init__(self):
        self.get_query = {}
        self.query = {}
        self.jso = None


def do_headers(self, content_type):
    self.send_response(200)
    self.send_header('Access-Control-Allow-Origin', '*')
    self.send_header('Access-Control-Allow-Methods', 'GET, PUT, POST, OPTIONS')
    self.send_header("Access-Control-Allow-Headers", "version, X-Requested-With, Content-Type")
    self.send_header('Content-type', content_type)
    self.end_headers()


def check_key(query, key):
    key2 = "-" + key
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


def multi_post_params(self):
    content_length = int(self.headers['Content-Length'])
    f = self.rfile.read(content_length)
    u = f.decode("UTF8")
    jsons = json.loads(u)
    results = []
    for jso in jsons:
        results.append(get_query_from_json(jso))
    return results


def get_query_from_json(jso):
    result = QueryClass()
    result.jso = jso
    for field in list(result.jso.keys()):
        if field == "markup":
            for f in list(result.jso[field].keys()):
                result.query[f] = [str(result.jso[field][f])]
        else:
            if field != "state" and field != "input": result.query[field] = [str(result.jso[field])]
    # print(jso)
    return result


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


def query_params_to_map(query):
    """
    Returns query as a flattened map where all params string by - is removed
    :type query: Dict
    :rtype: Dict
    :param query: get params
    :return: flattened map of params
    """
    result = {}
    for field in query.keys():
        if not field.startswith("-"): result[field] = query[field][0]

    return result


def query_params_to_json(query):
    """
    Returns flattened json object from all params without those starting by -
    :type query: Dict
    :param query: get params
    :return: params as flattened json object
    """
    result = json.dumps(query_params_to_map(query))
    return result


def get_params(self):
    result = QueryClass()
    result.get_query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    result.query = parse_qs(urlparse(self.path).query, keep_blank_values=True)
    return result


def post_params(self):
    content_length = int(self.headers['Content-Length'])
    content_type = "application/json"
    if 'Content-Type' in self.headers: content_type = self.headers['Content-Type']
    if 'content-type' in self.headers: content_type = self.headers['content-type']

    f = self.rfile.read(content_length)
    print(f)
    u = f.decode("UTF8")

    result = QueryClass()
    result.query = {}

    if len(u) == 0: return result
    if content_type.find("json") < 0:  # ei JSON
        q = parse_qs(urlparse('k/?' + u).query, keep_blank_values=True)
        for field in list(q.keys()):
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

