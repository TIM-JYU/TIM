import json
import os
from typing import Dict, Any, Optional, Union

import requests
import yaml

from languages import Language
from tim_common.cs_sanitizer import tim_sanitize

JSXGRAPHAPI_START = "[[jsxgraphapi"
JSXGRAPHAPI_END = "[[/jsxgraphapi]]"
JSXGRAPHAPI_BLOCK_PREFIX = "jsxgraphapi_tmp"


def do_jsxgraph_replace(q):
    q = q.replace('[[jsxgraph ', '[[jsxgraphapi ')
    q = q.replace('[[jsxgraph]]', '[[jsxgraphapi]]')
    q = q.replace('[[/jsxgraph]]', '[[/jsxgraphapi]]')
    return q


STACK_API_SERVER_ADDRESS = os.environ.get("STACK_API_SERVER") or "stack-api-server"


class Stack(Language):
    ttype = "stack"

    def can_give_task(self):
        return True

    def runner_name(self):
        return "stack-runner"

    @staticmethod
    def js_files():
        return ["/cs/js/build/stack.js"]  # , "/cs/stack/ServerSyncValues.js"]

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False

    def modify_usercode(self, s):
        if not s.startswith("{"):
            return s
        s = s.replace("&quot;", '"')
        js = json.loads(s)
        res = ''
        for key in js:
            res += js[key] + "\n"
        return res

    def run(self, result, sourcelines, points_rule):
        get_task = self.query.jso.get("input", {}).get("getTask", False)
        url = f"http://{STACK_API_SERVER_ADDRESS}/api/endpoint.php"
        data = self.query.jso.get("input").get("stackData")
        markup = self.query.jso.get('markup')
        stack_data = markup.get('-stackData')
        if not stack_data:
            stack_data = markup.get('stackData')
        if not stack_data:
            err = "stackData missing from plugin"
            return 0, "", err, ""
        seed = stack_data.get("seed", 0)
        userseed = seed
        state = self.query.jso.get("state", {})
        input = self.query.jso.get('input', {})
        ask_new = input.get("askNew", False)
        if isinstance(state, dict) and not ask_new:
            # if state.get('usercode') == input.get('usercode'):
            if not get_task:
                userseed = state.get("seed", seed)
        nosave = input.get('nosave', False)
        stack_data["seed"] = userseed

        q = stack_data.get("question", "")
        q_data = self.parse_stack_question(q, not self.query.jso.get('markup').get('stackjsx'))
        stack_data["question"] = q_data

        if nosave or get_task:
            stack_data['score'] = False
            stack_data['feedback'] = False
        stack_data["answer"] = data.get("answer")
        stack_data["prefix"] = data.get("prefix")
        stack_data["verifyvar"] = data.get("verifyvar", "")
        stack_data["ploturl"] = '/stackserver/plots/'
        if stack_data["verifyvar"]:
            stack_data["score"] = False
        else:
            save = result["save"]
            # if not new_task:
            save["seed"] = userseed

        r = requests.post(url=url, data=json.dumps(stack_data))  # json.dumps(data_to_send, cls=TimJsonEncoder))
        # r = requests.get(url="http://stack-test-container/api/endpoint.html")

        try:
            r = r.json()
        except json.JSONDecodeError:
            return 1, "", str(r.content.decode()), ""
        out = "Score: %s" % r.get("score", 0)
        # r['questiontext'] = tim_sanitize(r['questiontext'])

        if nosave:
            out = ""
            result["nosave"] = True
        web = result["web"]
        web["stackResult"] = r
        return 0, out, "", ""

    def convert(self, sourcelines):
        url = "http://stack-api-server/api/xmltoyaml.php"
        data = {'xml': sourcelines}
        r = requests.post(url=url, data=json.dumps(data))
        r = r.json()
        return 0, r.get('yaml'), "", ""

    def parse_stack_question(self, stack_question: Union[str, Dict[str, Any]], replace_jsxgraph_blocks: bool = True):
        if not stack_question:
            return {}
        if isinstance(stack_question, str):
            # Convert XML Stack question to YAML before parsing
            # This check is the same as in stack/api/endpoint.php
            if stack_question.strip().startswith("<"):
                _, stack_question, _, _ = self.convert(stack_question)

            try:
                question = yaml.safe_load(stack_question)
            except yaml.YAMLError:
                return {}

            if not question or not isinstance(question, dict):
                return {}
        else:
            question = stack_question

        jsxgraph_block_cache = {}

        for k, v in question.items():
            # Multilang questions can have multiple question_html* attributes, handle jsxgraph properly for them all
            if not k.startswith("question_html"):
                continue
            q_html = v
            jsxgraph_block_cache[k] = {}
            if replace_jsxgraph_blocks:
                q_html = do_jsxgraph_replace(q_html)
                question[k] = q_html

            # Prevent [[jsxgraphapi]] blocks from being sanitized as Bleach will
            # sanitize away some characters from it (e.g. < and >)
            # Do this by extracting the blocks, pass data to sanitizer and then reinsert the blocks
            while True:
                block = value_between(q_html, JSXGRAPHAPI_START, JSXGRAPHAPI_END)
                if not block:
                    break
                key = f"{JSXGRAPHAPI_BLOCK_PREFIX}_{hash(block)}"
                jsxgraph_block_cache[k][key] = block
                q_html = q_html.replace(block, key)
            question[k] = q_html

        sanitize_stack_question(question)

        for k, blocks in jsxgraph_block_cache.items():
            q_html = question[k]
            for block_key, block in blocks.items():
                q_html = q_html.replace(block_key, block)
            question[k] = q_html

        return question


def value_between(s: str, start: str, end: str) -> Optional[str]:
    start_index = s.find(start, 0)
    if start_index < 0:
        return None
    end_index = s.find(end, start_index)
    if end_index < 0:
        return None
    return s[start_index:end_index + len(end)]


def sanitize_stack_question(d):
    for k, v in d.items():
        # Only sanitize data that stack actually considers as HTML (properties that end with _html)
        # Sanitizing anything else can cause issues with Maxima (e.g. sanitizing < to &lt;)
        if isinstance(k, str) and "_html" in k and isinstance(v, str):
            d[k] = tim_sanitize(v)
        elif isinstance(v, dict):
            sanitize_stack_question(v)
