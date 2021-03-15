import json
from typing import Dict, Callable, Any, Optional

import requests

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
        url = "http://stack-api-server/api/endpoint.php"
        data = self.query.jso.get("input").get("stackData")
        stack_data = self.query.jso.get('markup').get('-stackData')
        if not stack_data:
            stack_data = self.query.jso.get('markup').get('stackData')
        if not stack_data:
            err = "stackData missing from plugin"
            return 0, "", err, ""
        seed = stack_data.get("seed", 0)
        userseed = seed
        state = self.query.jso.get("state", {})
        if isinstance(state, dict):
            userseed = state.get("seed", seed)
        nosave = self.query.jso.get('input', {}).get('nosave', False)
        stack_data["seed"] = userseed
        q = stack_data.get("question", "")

        if not self.query.jso.get('markup').get('stackjsx'):
            transform_stack_question(stack_data, do_jsxgraph_replace)

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
            save["seed"] = userseed

        # Prevent [[jsxgraphapi]] blocks from being sanitized as Bleach will
        # sanitize away some characters from it (e.g. < and >)
        # Do this by extracting the blocks, pass data to sanitizer and then reinsert the blocks
        jsxgraph_blocks = collect_jsxgraphapi_blocks(stack_data)
        sanitize_dict(stack_data)
        restore_jsxgraphapi_blocks(stack_data, jsxgraph_blocks)

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


def value_between(s: str, start: str, end: str) -> Optional[str]:
    start_index = s.find(start, 0)
    if start_index < 0:
        return None
    end_index = s.find(end, start_index)
    if end_index < 0:
        return None
    return s[start_index:end_index + len(end)]


def collect_jsxgraphapi_blocks(stack_data: Any) -> Dict[str, str]:
    result = {}

    def collect(q: str) -> str:
        while True:
            block = value_between(q, JSXGRAPHAPI_START, JSXGRAPHAPI_END)
            if not block:
                break
            key = f"{JSXGRAPHAPI_BLOCK_PREFIX}_{hash(block)}"
            result[key] = block
            q = q.replace(block, key)
        return q

    transform_stack_question(stack_data, collect)
    return result


def restore_jsxgraphapi_blocks(stack_data: Any, jsxgraph_blocks: Dict[str, str]) -> None:
    def restore(q: str) -> str:
        for k, v in jsxgraph_blocks.items():
            q = q.replace(k, v)
        return q

    transform_stack_question(stack_data, restore)


def transform_stack_question(stack_data: Any, transform: Callable[[str], str]) -> None:
    q = stack_data.get("question", "")
    if isinstance(q, str):
        stack_data["question"] = transform(q)
    else:
        q_html = stack_data["question"]["question_html"]
        if q_html:
            stack_data["question"]["question_html"] = transform(q_html)


def sanitize_dict(d):
    for k, v in d.items():
        if isinstance(v, str):
            d[k] = tim_sanitize(v)
        elif isinstance(v, dict):
            sanitize_dict(v)
