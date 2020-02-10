import json
import os
from languages import Language, get_by_id

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

JSREADYHTML['simpleDrawIO'] = """
<!doctype html>
<html>
<head>
<head>
	<style type="text/css">
		html, body, #wrapper {
			height:90%;
			width:100%;
			margin:0;
			padding:0;
			border:0;
		}
		table#wrapper {
			height:75%;
		}
		#wrapper td {
			vertical-align:middle;
			text-align:center;
			cursor:pointer;
		}
		iframe {
			border:0;
			position:fixed;
			top:0;
			left:0;
			right:0;
			bottom:0;
			width:100%;
			height:100%
		}
	</style>
	<script type="text/javascript">
	    var TIMJS = {};
		var editor = 'https://www.draw.io/?embed=1&ui=atlas&spin=1&proto=json&configure=1';
		var initial = null;
		var name = null;

		function edit(elt)
		{
		    elt =  document.getElementById('diagram');
			var iframe = document.createElement('iframe');
			iframe.setAttribute('frameborder', '0');

			var close = function()
			{
				window.removeEventListener('message', receive);
				document.body.removeChild(iframe);
			};

			var draft = null; // localStorage.getItem('.draft-' + name);
						
			if (draft != null)
			{
				draft = JSON.parse(draft);
							
				if (!confirm("A version of this page from " + new Date(draft.lastModified) + " is available. Would you like to continue editing?"))
				{
					draft = null;
				}
			}
			
			var receive = function(evt)
			{
				if (evt.data.length > 0)
				{
					var msg = JSON.parse(evt.data);
					
					// If configure=1 URL parameter is used the application
					// waits for this message. For configuration options see
					// https://desk.draw.io/support/solutions/articles/16000058316
					if (msg.event == 'configure')
					{
						// Configuration example
						iframe.contentWindow.postMessage(JSON.stringify({action: 'configure',
							config: {defaultFonts: ["Humor Sans", "Helvetica", "Times New Roman"]}}), '*');
					}
					else if (msg.event == 'init')
					{
						if (draft != null)
						{
							iframe.contentWindow.postMessage(JSON.stringify({action: 'load',
								autosave: 1, xml: draft.xml}), '*');
							iframe.contentWindow.postMessage(JSON.stringify({action: 'status',
								modified: true}), '*');
						}
						else
						{
							// Avoids unescaped < and > from innerHTML for valid XML
							var svg = new XMLSerializer().serializeToString(elt.firstChild);
							iframe.contentWindow.postMessage(JSON.stringify({action: 'load',
								autosave: 1, xml: svg}), '*');
						}
					}
					else if (msg.event == 'export')
					{
						// Extracts SVG DOM from data URI to enable links
						var svg = atob(msg.data.substring(msg.data.indexOf(',') + 1));
						elt.innerHTML = svg;
						// localStorage.setItem(name, JSON.stringify({lastModified: new Date(), data: svg}));
						// localStorage.removeItem('.draft-' + name);
						draft = null;
						close();
					}
					else if (msg.event == 'autosave')
					{
						// localStorage.setItem('.draft-' + name, JSON.stringify({lastModified: new Date(), xml: msg.xml}));
					}
					else if (msg.event == 'save')
					{
						iframe.contentWindow.postMessage(JSON.stringify({action: 'export',
							format: 'xmlsvg', xml: msg.xml, spin: 'Updating page'}), '*');
						// localStorage.setItem('.draft-' + name, JSON.stringify({lastModified: new Date(), xml: msg.xml}));
					}
					else if (msg.event == 'exit')
					{
						// localStorage.removeItem('.draft-' + name);
						draft = null;
						close();
					}
				}
			};

			window.addEventListener('message', receive);
			iframe.setAttribute('src', editor);
			document.body.appendChild(iframe);
		};
				
		function load()
		{
			initial = document.getElementById('diagram').innerHTML;
			start();
		};
		
		function start()
		{
			name = (window.location.hash.length > 1) ? window.location.hash.substring(1) : 'default';
			var current = null; // localStorage.getItem(name);
		
			if (current != null)
			{
				var entry = JSON.parse(current);
				document.getElementById('diagram').innerHTML = entry.data;
			}
			else
			{
				document.getElementById('diagram').innerHTML = initial;
			}
		};
		
		var globaldata = null;
		window.onload = function() {
		    if ( TIMJS.initData ) globaldata = TIMJS.initData;
		    setData(globaldata);
		    load();
		}
		
		window.addEventListener('hashchange', start);
		function getData() {
		    var data = document.getElementById('diagram').innerHTML; 
		    return {'c': data};
		}
		function setData(data) {
		    if ( !data || !data.c ) return;
		    document.getElementById('diagram').innerHTML = data.c;
		}
	</script>

<!--<script src="##TIM_HOST##/csstatic/drawio/drawio.js"></script>-->
 
</head>
<body>
<div>
<button onclick="edit(this);">Muokkaa</button>
<div id="wrapper">
<div id="diagram"><svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" version="1.1" width="1px" height="1px" viewBox="-0.5 -0.5 1 1" content="<mxfile host=&quot;www.draw.io&quot; modified=&quot;2020-02-09T18:51:35.659Z&quot; agent=&quot;Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36&quot; etag=&quot;IIS9Wlu7M_3kGZSahqxD&quot; version=&quot;12.6.6&quot;><diagram id=&quot;14058d99-db58-e9f5-3aba-399879c13e25&quot; name=&quot;Page-1&quot;>dZHBEoIgEIafhrvC5OjZrC6dPHQmQWRC10EcradPA1PG4sAs37+7PyyIpPV41rStrsC4QjhgIyJHhHESRdM+g6cFhzi0QGjJLNqAXL64g4GjvWS88xINgDKy9WEBTcML4zGqNQx+WgnKd22p4DuQF1Tt6U0yU1ka42jlFy5FtTiHUWKVOy0eQkPfOD+ESflZVq7p0ss9tKsog2GDSIZIqgGMjeox5Woe7TI2W3f6o37vrXljfhRMwdp7Onj/R7I3</diagram></mxfile>" style="background-color: rgb(255, 255, 255);"><defs></defs><g></g></svg></div>
</div>
</div>
<script>
//OPTIONS	// TIMJS
//ORIGINALDATA
//JAVASCRIPT
</script>
</body>
</html>
"""

class DrawIO(JSframe):
    def modify_query(self):
        ma = self.query.jso['markup']
        readyhtml = get_by_id(ma, "readyHtml", None)
        srchtml = get_by_id(ma, "srchtml", None)
        if readyhtml is None and srchtml is None:
            ma["readyHtml"] = "simpleDrawIO"
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
