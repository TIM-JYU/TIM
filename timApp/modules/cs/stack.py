import json

import requests

from cs_sanitizer import tim_sanitize
from languages import Language


def do_jsxgraph_replace(q):
    q = q.replace('[[jsxgraph ', '[[jsxgraphapi ')
    q = q.replace('[[jsxgraph]]', '[[jsxgraphapi]]')
    q = q.replace('[[/jsxgraph]]', '[[/jsxgraphapi]]')
    return q


class Stack(Language):
    def can_give_task(self):
        return True

    def runner_name(self):
        return "stack-runner"

    def js_files(self):
        return ["/cs/js/build/stack.js", "/cs/stack/ServerSyncValues.js"]

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
            if isinstance(q, str):
                stack_data["question"] = do_jsxgraph_replace(q)
            else:
                q_html = stack_data["question"]["question_html"]
                if q_html:
                    stack_data["question"]["question_html"] = do_jsxgraph_replace(q_html)

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

        sanitize_dict(stack_data)

        # TODO: Couldn't stack server accept dict directly in 'question'?
        if isinstance(q, dict):
            q = json.dumps(q)
            stack_data["question"] = q

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


def sanitize_dict(d):
    for k, v in d.items():
        if isinstance(v, str):
            d[k] = tim_sanitize(v)
        elif isinstance(v, dict):
            sanitize_dict(v)
