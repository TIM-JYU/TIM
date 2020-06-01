import json
import os

import re

from collections import Iterable

from languages import Language, get_by_id
from typing import Match

JSREADYHTML = {}

JSREADYOPTIONS = {}


class JSframe(Language):
    def get_default_before_open(self):
        return '<div class="defBeforeOpen"><p>Open JS-frame</p></div>'

    def runner_name(self):
        return "jsframe-runner"

    def js_files(self):
        return ["/cs/js/build/jsframe.js"]

    def can_give_task(self):
        return True

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = "/tmp/%s/%s.txt" % (self.basename, self.filename)
        self.fileext = "txt"
        self.readpoints_default = 'Score: (.*)'
        self.delete_tmp = False
        self.jsobject = 'window.'

    def modify_usercode(self, s):
        return s

    def run(self, result, sourcelines, points_rule):
        self.save(result)
        return 0, "saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if 'type' in data:
            del data['type']
        result["save"] = data
        return 0, "saved", "", ""

    def deny_attributes(self):
        return {  # "srchtml":"",
            "commands": "",
            "filename": "",
            "javascript": "",
            "options": "",
            "prehtml": "",
            "posthtml": "",
            'readyHtml': "",
            'readyOptions': "",
        }

    def state_copy(self):
        return ["c"]

    def iframehtml(self, result, sourcelines, points_rule):
        self.modify_query()
        ma = self.query.jso['markup']
        srchtml = get_by_id(ma, 'srchtml', '')
        data = ma.get('data', None)
        fielddata = ma.get('fielddata', None)

        state = self.query.jso.get("state", {})
        if state:
            c = state.get("c", None)
            if c is not None:
                data = c
        init_data = ""
        if data:
            init_data = self.jsobject + "initData = " + json.dumps(data) + ";\n"
        if fielddata:
            init_data += self.jsobject + "fieldData = " + json.dumps(fielddata) + ";\n"
        if init_data:
            init_data = "<script>" + init_data + "</script>"
            srchtml = srchtml.replace("</body>", init_data + "\n</body>")

        return srchtml

    def modify_query(self):
        ma = self.query.jso['markup']
        readyhtml = get_by_id(ma, "readyHtml", "")
        readyoptions = get_by_id(ma, "readyOptions", readyhtml)
        src = ma.get("srchtml", "")
        if not src and readyhtml:
            src = JSREADYHTML.get(readyhtml, "")
        if src.find("TIMJS") >= 0:
            self.jsobject = "TIMJS."

        opt = get_by_id(ma, "options", None)
        if not opt:
            opt = JSREADYOPTIONS.get(readyoptions, None)
        if opt:
            src = src.replace("//OPTIONS", self.jsobject + "options = " + json.dumps(opt) + ";")

        contstyle = 'style="' + get_by_id(ma, "contStyle", 'width: 100%; margin: auto; ') + '"'
        src = src.replace("CONTSTYLE", contstyle)

        src = src.replace("##TIM_HOST##", os.environ['TIM_HOST'])

        original_data = get_by_id(ma, "data", None)
        if original_data:
            src = src.replace("//ORIGINALDATA", self.jsobject + "originalData = " + json.dumps(original_data) + ";")

        javascript = get_by_id(ma, "javascript", None)
        if javascript:
            src = src.replace("//JAVASCRIPT", javascript)

        src = src.replace('##CHARTJSVERSION##', ma.get('chartjsversion', '2.8.0'))
        ma["srchtml"] = src
        return


JSREADYHTML['oneDataChartJS'] = """
<!doctype html>
<html>
<head>
<script src="https://www.chartjs.org/dist/##CHARTJSVERSION##/Chart.min.js"></script>
<script src="##TIM_HOST##/csstatic/chartjs/timonedata.js"></script> 
</head>
<body>
<div id="container" CONTSTYLE>
<canvas id="canvas"></canvas>
</div>
<script>
//OPTIONS	// TIMJS
//ORIGINALDATA
//JAVASCRIPT
</script>
</body>
</html>
"""

class ChartJS(JSframe):
    def modify_query(self):
        ma = self.query.jso['markup']
        readyhtml = get_by_id(ma, "readyHtml", None)
        srchtml = get_by_id(ma, "srchtml", None)
        if readyhtml is None and srchtml is None:
            ma["readyHtml"] = "oneDataChartJS"
        height = ma.get("height", None)  # Automatic aspect ratio or height
        data = ma.get("data", {})
        dopt =  data.get("options", {})
        ar = data.get("aspectRatio", dopt.get('aspectRatio', None))
        if height and not ar:
            if not ma.get("data", None):
                ma["data"] = {}
            data["aspectRatio"] = ma.get("width", 800) / (height * 0.95)
        if not height and ar:
            ma["height"] = ma.get("width", 800) / (ar * 0.95)
        super().modify_query()
        return


with open('jsframehtml/simpleDrawIO.html', 'r', encoding='utf-8') as f:
    JSREADYHTML['simpleDrawIO'] = f.read()

# see: https://regex101.com/r/eEPcs2/1/
# regexp to find text's inside svg
SVGTEXT_PROG = re.compile(">([^>]*)</text")

class DrawIO(JSframe):
    def modify_query(self):
        """
        state = self.query.jso.get('state', None)
        if state:
            c = state.get('c', None)
            if c:
                state['c'] = bytes(c, 'ISO-8859-1').decode('utf-8') # TODO: miksi näin pitää tehdä???
        """
        ma = self.query.jso['markup']
        readyhtml = get_by_id(ma, "readyHtml", None)
        srchtml = get_by_id(ma, "srchtml", None)
        if readyhtml is None and srchtml is None:
            ma["readyHtml"] = "simpleDrawIO"
        height = ma.get("height", None)  # Automatic aspect ratio or height
        data = ma.get("data", {})
        if isinstance(data, str):
            data = data.replace('xlink: ', 'xlink:')
            ma["data"] = data
            data = {}
        dopt = data.get("options", {})
        templates = ma.get("templates", "");
        ar = data.get("aspectRatio", dopt.get('aspectRatio', None))
        if height and not ar:
            if not ma.get("data", None):
                ma["data"] = {}
            data["aspectRatio"] = ma.get("width", 800) / (height * 0.95)
        if not height and ar:
            ma["height"] = ma.get("width", 800) / (ar * 0.95)
        ma["initListener"] = ma.get("initListener", True)
        ma["iframeopts"] = ma.get("iframeopts", 'sandbox="allow-scripts allow-same-origin allow-popups"')
        ma["saveButton"] = ma.get("saveButton", "")
        # TODO: prevent user options if thereis
        templates = ma.get("templates", "")
        if isinstance(templates, str):
            templates = templates.replace('</mxlibrary>', '')
            templates = templates.replace('<mxlibrary>', '')
            try:
                templates = json.loads(templates)
            except json.decoder.JSONDecodeError:
                pass

        ma["options"] = {'fullscreen': ma.get("fullscreen", True), 'templates': templates}
        super().modify_query()
        return


    def get_review(self, usercode):
        """
        return text to show when reviewing task
        """
        c = self.query.jso.get('state', {}).get('c', None)
        if not c:
            return None
        matches: Iterable[Match] = SVGTEXT_PROG.finditer(c)
        texts = ""
        line = ""
        for m in matches:
            text = m.group(1)
            try:
                d = chr(195)
                if text.find(d) >= 0:
                    text = bytes(text, 'ISO-8859-1').decode('utf-8') # TODO: miksi näin pitää tehdä???
            except:
                # let text be as it was
                text = text
            if text != "Viewer does not support full SVG 1.1":
                if len(line) + len(text) > 75:
                    texts += line + "\n"
                    line = ""
                line += text + ", "
        if line:
            texts += line
        if not texts:
            texts = "Labels: 0"
        return texts
