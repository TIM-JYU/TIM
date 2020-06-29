import json
from base64 import b64encode
from languages import Language, get_by_id, html_change

GEOGEBRA_DEFAULT_SRC_HTML = """
<!DOCTYPE html>
<html>
<head>
  <title>Apps with Toolbar: Graphing Calculator</title>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
</head>
<body>
<div>
<script type="text/javascript" src="https://cdn.geogebra.org/apps/deployggb.js"></script>
<script type="text/javascript" src="/cs/geogebra/timgeo.js"></script>

<script type="text/javascript">
function perspective(p){
    updateHelp(p);
    ggbApplet.setPerspective(p);
}

var ggbApplet;

var P = {
borderColor: "#FFFFFF",
preventFocus: true,
//GEOWIDTH
//GEOHEIGHT
//GEOMATERIALID
//GGBBASE64
//GEOFILENAME
};


P.getData = function(){ 
   return {"data": ggbApplet.getBase64()}; 
}

P.setDataInit = function (api, geostate) {
    timgeo.setState(api, geostate);
}

P.setData = function(geostate) {
    P.setDataInit(ggbApplet, geostate);
}

//GEOJAVASCRIPT

var geostate = GEOSTATE;

P.appletOnLoad = function(api) {
    if ( !P.setDataInit ) return;
    ggbApplet = api;
    var g = atob(geostate);
    var state = JSON.parse(g);
    // timgeo.setState(api, state);
    P.setDataInit(api, state);
}



var applet = new GGBApplet(P, '5.0', 'geogebra_container');
//  when used with Math Apps Bundle, uncomment this:
//  applet.setHTML5Codebase('GeoGebra/HTML5/5.0/web3d/');

  window.onload = function() { applet.inject('geogebra_container');
}


function getData(){ 
    if ( P.getData )
        return P.getData();
}

function setData(geostate) {
    if ( P.setData )
        P.setData(geostate);
}

</script>

GEOPREHTML

<div id="geogebra_container"></div>

GEOPOSTHTML
</div>
</body>
</html>
"""

GEOGEBRA_PARAMETERS_INIT = """
// P.appName = "graphing";
P.width = 1200;
P.height = 600;
P.showMenuBar = true;
P.showAlgebraInput = true;
P.showToolBar = true;
P.customToolBar = "0 77 73 62 | 1 501 67 , 5 19 , 72 75 76 | 2 15 45 , 18 65 , 7 37 | 4 3 8 9 , 13 44 , 58 , 47 | 16 51 64 , 70 | 10 34 53 11 , 24  20 22 , 21 23 | 55 56 57 , 12 | 36 46 , 38 49  50 , 71  14  68 | 30 29 54 32 31 33 | 25 17 26 60 52 61 | 40 41 42 , 27 28 35 , 6";
P.showToolBarHelp = true;
P.showResetIcon = true;
P.enableLabelDrags = true;
P.enableShiftDragZoom = true;
P.enableRightClick = false;
P.errorDialogsActive = false;
P.useBrowserForJS = false;
P.allowStyleBar = false;
P.preventFocus = true;
P.showZoomButtons = true;
P.capturingThreshold = 3;
{}
"""

GEOGEBRA_TOOL_HTML = """
<script type="text/javascript">
function objName() { return document.getElementById("objName").value; }
function resultArea() { return document.getElementById("resultArea"); }
function setArea(s) { resultArea().value = s; }
function helpArea() { return document.getElementById("helpText"); }
function setHelp(s) { helpArea().value = s; }
var api;
function evalJS(s) {
   api = ggbApplet;
   try {
       var res = eval(s);
       if ( res ) return res;
       return "";
   } catch(err) {
       return err.message;
   }
}
function loadMaterial() {
    let mat = document.getElementById("materialId").value;
    if ( mat.indexOf("/") >= 0 ) {
        P['filename'] = mat;
        delete P['material_id'];
        setHelp('"filename": "' + mat +'"')
    } else {
        P['material_id'] = mat;
        delete P['filename'];
        setHelp('"material_id": "' + mat +'"')
    }
    delete P['ggbBase64'];
    applet = new GGBApplet(P, '5.0', 'geogebra_container');
    applet.inject('geogebra_container');
}
</script>

<p>Eval code:
<textarea name="evalarea" id="evalarea" cols=40 rows=3></textarea>
Eval:
<a href="javascript:;" onclick="ggbApplet.evalCommand(document.getElementById('evalarea').value); setHelp('ggbApplet.evalCommand(cmds)')">commands</a>
<a href="javascript:;" onclick="setArea(ggbApplet.evalCommandGetLabels(document.getElementById('evalarea').value)); setHelp('ggbApplet.evalCommandGetLabels(cmds)')">labels</a>
<a href="javascript:;" onclick="setArea(ggbApplet.evalCommandCAS(document.getElementById('evalarea').value)); setHelp('ggbApplet.evalCommandCAS(cmds)')">CAS</a>
<a href="javascript:;" onclick="setArea(evalJS(document.getElementById('evalarea').value)); setHelp('eval(cmds)')">JS</a>
</p>

<ul>
  <li>Get constraction:
    <a href="javascript:;" onclick="setArea(ggbApplet.getBase64()); setHelp('ggbApplet.getBase64()')">GGB</a>
    <a href="javascript:;" onclick="setArea(ggbApplet.getXML()); setHelp('ggbApplet.getXML()')">XML</a>
    <a href="javascript:;" onclick="setArea(ggbApplet.getAllObjectNames()); setHelp('ggbApplet.getAllObjectNames()')">Names</a>
    <a href="javascript:;" onclick="setArea(timgeo.getConstructionState(ggbApplet)); setHelp('timgeo.getConstructionState(ggbApplet)')">State</a>
    <a href="javascript:;" onclick="setArea(timgeo.getCommands(ggbApplet)); setHelp('timgeo.getCommands(ggbApplet)')">Commands</a>
  </li>
  <li>Obj name:
    <input id="objName" size="6"/>
    Get obj:
    <a href="javascript:;" onclick="setArea(timgeo.getObjXML(ggbApplet,objName())); setHelp('timgeo.getObjXML(ggbApplet,\\''+objName()+'\\')')">XML</a>
    <a href="javascript:;" onclick="setArea(timgeo.getObjCommand(ggbApplet,objName())); setHelp('timgeo.getObjCommand(ggbApplet,\\''+objName()+'\\')')">Command</a>
    Value:
    <a href="javascript:;" onclick="setArea(timgeo.getNumberValue(ggbApplet,objName())); setHelp('timgeo.getNumberValue(ggbApplet,\\''+objName()+'\\')')">Number</a>
    <a href="javascript:;" onclick="setArea(timgeo.getObjValue(ggbApplet,objName())); setHelp('timgeo.getObjValue(ggbApplet,\\''+objName()+'\\')')">String</a>
    <a href="javascript:;" onclick="setArea(timgeo.getPureValue(ggbApplet,objName())); setHelp('timgeo.getPureValue(ggbApplet,\\''+objName()+'\\')')">Pure</a>
  </li>
</ul>
  JS: <input id="helpText" title="Javascript command" size="70"/>
  <a href="javascript:timgeo.copyArea(helpArea())">Copy</a>
<br>
<textarea name="resultArea" id="resultArea" cols=90 rows=10></textarea>
<ul>
   <li><a href="javascript:timgeo.copyArea(resultArea())">Copy area</a></li>
   <li>Set constraction:
      <a href="javascript:;" onclick="ggbApplet.setBase64(resultArea().value); setHelp('ggbApplet.setBase64(ggb)')">GGB</a>
      <a href="javascript:;" onclick="ggbApplet.setXML(resultArea().value); setHelp('ggbApplet.setXML(xml)')">XML</a>
      <a href="javascript:;" onclick="ggbApplet.reset(); setHelp('ggbApplet.reset()')">Reset</a>
      &nbsp;<input id="materialId" size="45" title="Write material ID or GGB-file URL"/>
      <a href="javascript:;" onclick="loadMaterial()">Load</a>
   </li>
   <li><a href="javascript:;" onclick="ggbApplet.evalXML(resultArea().value); setHelp('ggbApplet.evalXML(xml)')">Set Obj XML</a></li>
</ul>
"""

class Geogebra(Language):
    ttype="geogebra"
    global GEOGEBRA_DEFAULT_SRC_HTML
    global GEOGEBRA_PARAMETERS_INIT
    global GEOGEBRA_TOOL_HTML

    def get_default_before_open(self):
        return '<div class="defBeforeOpen"><p>Open GeoGebra</p></div>'

    @staticmethod
    def js_files():
        return ["/cs/js/build/geogebra.js"]

    def can_give_task(self):
        return True

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
        self.save(result)
        return 0, "GeoGebra saved", "", ""

    def save(self, result):
        data = dict(self.query.jso["input"])
        if 'type' in data:
            del data['type']
        result["save"] = data
        return 0, "GeoGebra saved", "", ""

    def deny_attributes(self):
        return {"srchtml":"",
                "filename": "",
                "prehtml": "",
                "posthtml": "",
                "data":"",
                "javascript": "",
                "commands": "",
                "objxml": ""
        }

    def state_copy(self):
        return ["message"]

    def iframehtml(self, result, sourcelines, points_rule):
        ma = self.query.jso['markup']
        jsformat = "{}"
        jsdef = None
        postdef = None
        if get_by_id(ma, 'tool', False):
            jsformat = GEOGEBRA_PARAMETERS_INIT
            jsdef = ""
            postdef = GEOGEBRA_TOOL_HTML

        srchtml = get_by_id(ma, 'srchtml', GEOGEBRA_DEFAULT_SRC_HTML)
        srchtml = html_change(srchtml, "//GEOMATERIALID", ma, "material_id", 'material_id: "{}",', None)
        srchtml = html_change(srchtml, "//GEOFILENAME", ma, "filename", 'filename: "{}",', None)
        srchtml = html_change(srchtml, "//GEOWIDTH", ma, "width", 'width: {},', "", "800", -10)
        srchtml = html_change(srchtml, "//GEOHEIGHT", ma, "height", 'height: {},', "", "450", -20)  # prevent scrollbar
        srchtml = html_change(srchtml, "//GEOJAVASCRIPT", ma, "javascript", jsformat, None, jsdef)
        srchtml = html_change(srchtml, "GEOPREHTML", ma, "prehtml", '{}', "")
        srchtml = html_change(srchtml, "GEOPOSTHTML", ma, "posthtml", '{}', "", postdef)

        data = get_by_id(ma, "data", "")
        state = self.query.jso.get("state", {})
        if state is None:
            state = {}
        udata = state.get("data", None)
        if udata:
            data = udata
        if data:
            if data[0] == '<': # is XML?
                if not udata:  # send XML in data part
                    state["data"] = data
                data = ''
            else:
                state["data"] = ''
        if data: # send ggb in parameters ggbBase64
            srchtml = srchtml.replace('//GGBBASE64', f'ggbBase64: "{data.strip()}",')

        commands = get_by_id(ma, "commands", None)
        if commands:
            state["commands"] = commands
        objxml = get_by_id(ma, "objxml", None)
        if objxml:
            state["objxml"] = objxml
        geostate = b64encode(json.dumps(state).encode("UTF-8")).decode().strip()
        srchtml = srchtml.replace('GEOSTATE', f"'{geostate}'")
        return srchtml

