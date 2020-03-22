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

JSREADYHTML['simpleDrawIO'] = """
<!doctype html>
<html>
<head>
    <meta charset="UTF-8"> 
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
	    var TIMJS = {frm: true, options: {}};
		var editor = 'https://www.draw.io/?embed=1&ui=atlas&spin=1&proto=json&configure=1';
		var initial = null;
		var name = null;
		var drw = null;
		var iframe = null;

		function edit(elt)
		{
		    if ( TIMJS.options && TIMJS.options.fullscreen ) { 
		        TIMJS.frm = !TIMJS.options.fullscreen; 
		    } 
		    var fs = document.getElementById('fullscreen');
            if ( fs ) {
		        TIMJS.frm = !fs.checked; 
            }

		    
		    if ( drw && !drw.closed ) {
		        drw.focus();
		        return;
		    }
		    elt =  document.getElementById('diagram');
		    if ( TIMJS.frm ) {
			   var iframe = document.createElement('iframe');
			   iframe.setAttribute('frameborder', '0');
			}   

			var close = function()
			{
				window.removeEventListener('message', receive);
				if ( TIMJS.frm ) {
				    document.body.removeChild(iframe);
			    } else {
				    drw.close();
				}    
				drw = null;
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
						drw.postMessage(JSON.stringify({action: 'configure',
							config: {defaultFonts: ["Humor Sans", "Helvetica", "Times New Roman"]}}), '*');
					}
					else if (msg.event == 'init')
					{
						if (draft != null)
						{
							drw.postMessage(JSON.stringify({action: 'load',
								autosave: 1, xml: draft.xml}), '*');
							drw.postMessage(JSON.stringify({action: 'status',
								modified: true}), '*');
						}
						else
						{
							// Avoids unescaped < and > from innerHTML for valid XML
                            var ch = elt.firstChild;
							var svg = '';
							if ( ch ) svg = new XMLSerializer().serializeToString(ch);
							drw.postMessage(JSON.stringify({action: 'load',
								autosave: 1, xml: svg}), '*');
						}
					}
					else if (msg.event == 'export')
					{
						// Extracts SVG DOM from data URI to enable links
						var svg = atob(msg.data.substring(msg.data.indexOf(',') + 1));
						// elt.innerHTML = svg;
						setData({'c': svg});
						// localStorage.setItem(name, JSON.stringify({lastModified: new Date(), data: svg}));
						// localStorage.removeItem('.draft-' + name);
						draft = null;
						if ( window.port2 )	window.port2.postMessage({msg:"datasave", data: getData()});
						close();
					}
					else if (msg.event == 'autosave')
					{
						// localStorage.setItem('.draft-' + name, JSON.stringify({lastModified: new Date(), xml: msg.xml}));
					}
					else if (msg.event == 'save')
					{
						drw.postMessage(JSON.stringify({action: 'export',
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
			if ( TIMJS.frm ) {
			    iframe.setAttribute('src', editor);
			    document.body.appendChild(iframe);
			    drw = iframe.contentWindow;
			} else  {  
    			drw = window.open(editor);
    		}	
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
		    if ( !data || !data.c || Object.entries(data.c).length === 0 ) return;
		    let c = data.c;
		    let d = String.fromCharCode(195);  // UTF-Escape for ä
		    if ( c.indexOf(d) >= 0 )
		        c = decodeURIComponent(escape(c));
		    document.getElementById('diagram').innerHTML = c;
		}
	</script>

<!--<script src="##TIM_HOST##/csstatic/drawio/drawio.js"></script>-->
 
</head>
<body>
<div>
<button onclick="edit(this);">Muokkaa</button>
Fullscreen <input id="fullscreen" type="checkbox" />
<div id="wrapper">
<div id="diagram"></div>
</div>
</div>
<script>
//OPTIONS	// TIMJS
var fs = document.getElementById('fullscreen');
fs.checked = TIMJS.options.fullscreen;
//ORIGINALDATA
//JAVASCRIPT
</script>
</body>
</html>
"""

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
        dopt =  data.get("options", {})
        ar = data.get("aspectRatio", dopt.get('aspectRatio', None))
        if height and not ar:
            if not ma.get("data", None):
                ma["data"] = {}
            data["aspectRatio"] = ma.get("width", 800) / (height * 0.95)
        if not height and ar:
            ma["height"] = ma.get("width", 800) / (ar * 0.95)
        ma["initListener"] = ma.get("initListener", True)
        ma["iframeopts"] = ma.get("iframeopts", 'sandbox="allow-scripts allow-same-origin allow-popups"')
        # TODO: prevent user options if thereis
        ma["options"] = {'fullscreen': ma.get("fullscreen", True)}
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
            text = bytes(text, 'ISO-8859-1').decode('utf-8') # TODO: miksi näin pitää tehdä???
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
