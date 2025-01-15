import json
import re
from base64 import b64encode
from languages import get_by_id
from jslanguage import JSWithHTML, JSREADYHTML


class JSframe(JSWithHTML):
    ttype = "jsframe"
    load_original_data = True
    initListenerDefault = True

    def get_default_before_open(self):
        return '<div class="defBeforeOpen"><p>Open JS-frame</p></div>'

    def runner_name(self):
        return "jsframe-runner"

    @staticmethod
    def js_files():
        return ["/cs/js/build/jsframe.js"]

    def can_give_task(self):
        return True

    def __init__(self, query, sourcecode):
        super().__init__(query, sourcecode)
        self.sourcefilename = f"/tmp/{self.basename}/{self.filename}.txt"
        self.fileext = "txt"
        self.readpoints_default = "Score: (.*)"
        self.delete_tmp = False
        self.jsobject = "window."

    def modify_usercode(self, s):
        return s

    def run(self, result, sourcelines, points_rule):
        self.save(result)
        return 0, "saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if "type" in data:
            del data["type"]
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
            "readyHtml": "",
            "readyOptions": "",
        }

    def state_copy(self):
        return ["c"]

    def iframehtml(self, result, sourcelines, points_rule):
        """
        This method is called when task is saved and iframehtml: true.
        This is mainly for generating html with reply included.
        Used for example in GeoGebra, but not so much other jsframes.
        :param result: result object to be returned from save
        :param sourcelines: source code lines if any
        :param points_rule: points rule if any
        :return: html string
        """
        self.modify_query()
        ma = self.query.jso["markup"]
        srchtml = get_by_id(ma, "srchtml", "")
        data = ma.get("data", None)
        fielddata = ma.get("fielddata", None)

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
        super().modify_query()
        ma = self.query.jso["markup"]
        # Seem that initListener is better to be on by default
        init_listener = ma.get("initListener", None)
        if init_listener is None:
            ma["initListener"] = self.initListenerDefault


JSREADYHTML[
    "oneDataChartJS"
] = """
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


def check_aspect_ratio(ma):
    data = ma.get("data", {})
    if isinstance(data, str):
        data = {}
    height = ma.get("height", None)  # Automatic aspect ratio or height
    dopt = data.get("options", {})
    ar = data.get("aspectRatio", dopt.get("aspectRatio", None))
    if height and not ar:
        if not ma.get("data", None):
            ma["data"] = {}
        data["aspectRatio"] = ma.get("width", 800) / (height * 0.95)
    if not height and ar:
        ma["height"] = ma.get("width", 800) / (ar * 0.95)
    return data


class ChartJS(JSframe):
    ttype = "chartjs"
    initListenerDefault = False

    def modify_query(self):
        ma = self.query.jso["markup"]
        readyhtml = get_by_id(ma, "readyHtml", None)
        srchtml = get_by_id(ma, "srchtml", None)
        if readyhtml is None and srchtml is None:
            ma["readyHtml"] = "oneDataChartJS"
        check_aspect_ratio(ma)
        super().modify_query()
        return


with open("jsframehtml/simpleDrawIO.html", encoding="utf-8") as f:
    JSREADYHTML["simpleDrawIO"] = f.read()

# see: https://regex101.com/r/eEPcs2/1/
# regexp to find text's inside svg
SVGTEXT_PROG = re.compile(">([^>]*)</text")


class DrawIO(JSframe):
    ttype = "drawio"
    load_original_data = False

    def modify_query(self):
        """
        state = self.query.jso.get('state', None)
        if state:
            c = state.get('c', None)
            if c:
                state['c'] = bytes(c, 'ISO-8859-1').decode('utf-8') # TODO: miksi näin pitää tehdä???
        """
        ma = self.query.jso["markup"]
        hosturl = ma.get("hosturl", "https://embed.diagrams.net/")
        ma["hosturl"] = hosturl
        readyhtml = get_by_id(ma, "readyHtml", None)
        srchtml = get_by_id(ma, "srchtml", None)
        if readyhtml is None and srchtml is None:
            ma["readyHtml"] = "simpleDrawIO"
        # templates = ma.get("templates", "") not needed?
        check_aspect_ratio(ma)
        ma["initListener"] = ma.get("initListener", True)
        ma["saveButton"] = ma.get("saveButton", "")
        # TODO: prevent user options if thereis
        templates = ma.get("templates", "")
        if isinstance(templates, str):
            templates = templates.replace("</mxlibrary>", "")
            templates = templates.replace("<mxlibrary>", "")
            try:
                templates = json.loads(templates)
            except json.decoder.JSONDecodeError:
                pass
        ma["options"] = {
            "fullscreen": ma.get("fullscreen", True),
            "templates": templates,
            "hideOptionsBar": not ma.get("task", True),
        }
        super().modify_query()
        return

    def get_review(self, usercode):
        """
        return text to show when reviewing task
        """
        state = self.query.jso.get("state", {})
        if not state:
            return ""
        c = state.get("c", None)
        if not c:
            return ""
        if isinstance(c, str):
            # Find svg data from the saved graph
            delete_start_str = 'content="'
            delete_end_str = '/mxfile&gt;"'
            first_del = c.find(delete_start_str)
            last_del = c.find(delete_end_str)
            if last_del < 0:  # unescaped format
                delete_end_str = '/mxfile>"'
                lastdel = c.find(delete_end_str)
                c = c[0:first_del] + c[lastdel + len(delete_end_str) : len(c)]
                c = c.replace("<br>", "<br/>")
                c = c.replace("&nbsp;", "")
            else:
                c = c[0:first_del] + c[last_del + len(delete_end_str) : len(c)]
            # TODO: Add a way to inform the browser that review data is in image format
            c = "data:image/svg+xml;base64," + str(
                b64encode(c.encode("utf-8")), "utf-8"
            )
        return c
