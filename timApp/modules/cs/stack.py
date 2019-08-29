import json
import html
from cs_sanitizer import check_not_script
from languages import Language
import requests


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
        if not isinstance(q, str):
            q = json.dumps(q)
            stack_data["question"] = q

        if not self.query.jso.get('markup').get('stackjsx') and q.find("[[jsxgraph") >= 0:  # make jsxgraph replace
            q = q.replace('[[jsxgraph ', '[[jsxgraphapi ')
            q = q.replace('[[jsxgraph]]', '[[jsxgraphapi]]')
            q = q.replace('[[/jsxgraph]]', '[[/jsxgraphapi]]')
            stack_data["question"] = q

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

        for key in stack_data:
            s = stack_data[key]
            if True:  # TODO: here maybe a list of checked fields
                try:
                    check_not_script(s)
                except Exception as e:
                    result['nosave'] = True
                    return 1, "", str(e) + " " + str(key) + ": " + html.escape(str(s)), ""

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


