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
            init_data = "window.initData = " + json.dumps(data) + ";\n"
        if fielddata:
            init_data += "window.fieldData = " + json.dumps(fielddata) + ";\n"
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

        opt = get_by_id(ma, "options", None)
        if not opt:
            opt = JSREADYOPTIONS.get(readyoptions, "")
        contstyle = 'style="' + get_by_id(ma, "contStyle", 'width: 100%; margin: auto; ') + '"'

        src = src.replace("//OPTIONS", "var options = " + json.dumps(opt) + ";").replace("CONTSTYLE", contstyle)
        src = src.replace("##TIM_HOST##", os.environ['TIM_HOST'])

        original_data = get_by_id(ma, "data", None)
        if original_data:
            src = src.replace("//ORIGINALDATA", "var originalData = " + json.dumps(original_data) + ";")

        src = src.replace('##CHARTJSVERSION##', ma.get('chartjsversion', 'master'))
        ma["srchtml"] = src
        return

JSREADYHTML['oneDataChartJS'] = """
<!doctype html>
<html>
<head>
<script src="https://www.chartjs.org/dist/##CHARTJSVERSION##/Chart.min.js"></script>
<script src="##TIM_HOST##/csstatic/chartjs/chartjs-plugin-trendline.js"></script> 
</head>
<body>
<div id="container" CONTSTYLE>
<canvas id="canvas"></canvas>
</div>
<script>
//OPTIONS	
//ORIGINALDATA

var COLORS = [
    '#4dc9f6',
    '#f67019',
    '#f53794',
    '#537bc4',
    '#acc236',
    '#166a8f',
    '#00a950',
    '#58595b',
    '#8549ba'
];
var color = Chart.helpers.color;
function pros(od) {
    // return od;
    let max = Math.max(...od);
    let a = [];
    for (let i=0; i<od.length; i++) {
       a[i] = Math.round(od[i]*1000/max)/10;
    }   
    // console.log(max, a);
    return a;
}

function addData(datasets, datas, keys, dopros) {   
    let ci = 0;	
    for (const v of keys) {
       let d = 	{
            lineTension: 0,
            label: ''+v+  (dopros ? ' %' : ''),
            fill: false,
            backgroundColor: color(COLORS[ci]).alpha(0.5).rgbString(),
            borderColor: COLORS[ci++],
            // borderDash: [3,10],
            borderWidth: 1,
        }
        if ( ci > COLORS.length ) ci = 0;
        let od = datas[v];
        if ( !od || od.length == 0) continue;
        d.data = dopros ? pros(od) : od;
        datasets.push(d);
    }
}	


var globaldata =  {
}

window.onload = function() {
    if ( window.initData ) globaldata = initData; // mergeDeep(globaldata, window.initData);
    setData(globaldata);
}

var chart = null;

function ensureDataSets(datasets, n) {
   let diff = n - datasets.length;
   if ( diff <= 0 ) return;
   for (i = diff; i < n; i++ ) {
       datasets[i] = {}
       mergeDeep(datasets[i], datasets[i-1]);
   }
   
}

function setData(data) {
  try {
    globaldata = data;
    if ( window.originalData ) {
        var newData = {};
        if ( chart ) originalData.datas = null; // prevent another add
        mergeDeep(newData, originalData, '#'); // do not loose possible !
        mergeDeep(newData, data);
        data = newData;
    }
    if ( !chart ) {  
        let ar = data.aspectRatio || data.options && data.options.aspectRatio;
        if ( ar ) options.options.aspectRatio = ar;
        var ctx = document.getElementById('canvas').getContext('2d');
        chart = new Chart(ctx,options); 
    }
    var datasets = chart.config.data.datasets;
    var coptions = chart.config.options;
    var dopros = data.dopros || false;
    let fieldindex = data.fieldindex || 0;

    if ( data.type ) {
        chart.config.type = data.type;
        if ( data.type == "scatter" || data.linearx) {
           coptions.scales.xAxes = [{ type: 'linear', position: "bottom", scaleLabel: {labelString: "", display: true}}];
           coptions.scales.yAxes = [{ type: 'linear', position: "left",   scaleLabel: {labelString: "", display: true}}];
        }
        if ( data.type == "scatter" ) {
           datasets[0].showLine = false; // for version 2.8.0
        }
    }    
    if ( data.labels ) chart.data.labels = data.labels;
    if ( data.data ) {
       fieldindex++;
       datasets[0].data = dopros ? pros(data.data) : data.data;
    }
    if ( data.data2 || data.label2 || data.dataopt2 ) {
        ensureDataSets(datasets, 2);
        datasets[1].backgroundColor = 'rgba(0,127,0,0.5)';
		datasets[1].borderColor = '#080';
    }  
    if ( data.data2 ) {
       if ( fieldindex == 1 ) fieldindex = 2;
       datasets[1].data = dopros ? pros(data.data2) : data.data2;
    }   
    if ( data.label ) datasets[0].label = data.label;
    if ( data.title) chart.options.title.text = data.title; // Tämä pitää olla näin
    if ( data.xlabel ) coptions.scales.xAxes[0].scaleLabel.labelString = data.xlabel;
    if ( data.ylabel) coptions.scales.yAxes[0].scaleLabel.labelString = data.ylabel;
    if ( data.label && datasets.length > 1 ) coptions.legend.display = true; 
    if ( data.label2 ) {
        coptions.legend.display = true;
        datasets[1].label = data.label2;
    }     
    if (typeof data.legend != "undefined") {
        if ( data.legend == false ) {
           coptions.legend.display = false;
        } else if ( data.legend == true ) {
           coptions.legend.display = true;
        } else {
           coptions.legend.display = true;
           coptions.legend.position = data.legend;
        }   
    }  
    let fdata = data.fielddata || window.fieldData;
    if ( fdata ) {
       ensureDataSets(datasets, fieldindex+1);
  	   if ( fieldindex == 0 ) chart.data.labels = fdata.graphdata.labels;
       datasets[fieldindex].data = fdata.graphdata.data;  
       datasets[fieldindex].backgroundColor = 'rgba(255,0,0,0.5)';
  	   datasets[fieldindex].borderColor = '#F00';
    }
    if ( data.trend ) {  // TODO: for every data
        let w = data.trend;
        if ( w === true ) w = 2;
        datasets[0].trendlineLinear = {
            'style': "rgba(255,105,180, .8)",
            'lineStyle': "dotted|solid",
            'width': w
        };
    } 
    if ( data.datas ) {
        keys = data.datakeys || Object.keys(data.datas)
        addData(datasets, data.datas, data.datakeys, dopros);
    }

    if ( data.options ) { mergeDeep(chart.options, data.options); }
    if ( data.dataopt ) { mergeDeep(datasets[0], data.dataopt); }
    if ( data.dataopt2 ) { mergeDeep(datasets[1], data.dataopt2); }

    chart.update();
  } catch(err) {
     var cont = document.getElementById('container');
     var p = document.createElement("p");             
     var textnode = document.createTextNode(err.message);       
     p.appendChild(textnode);
     cont.insertBefore(p, cont.firstChild);
  }  
};

function getData() {
    return globaldata;
}
</script>
</body>
</html>
"""

JSREADYOPTIONS['oneDataChartJS'] = {
    'type': 'bar',
    'data': {
        'labels': [1,2,3,4,5,6],
        'datasets': [
            {
                'label': '',
                'lineTension': 0,
                'fill': False,
                'backgroundColor': 'rgba(0,0,255,0.5)',
                'borderColor': '#0000ff',
                'borderWidth': 1,
                'data': [4,5,6,2,3,10],
            },
        ]
    },
    'options': {
        'responsive': True,
        'legend': { 'display': False, 'position': 'right', },
        'title': { 'display': True, 'text': '' },
        'scales': {
            'xAxes': [{ 'position': 'bottom', 'scaleLabel': {'labelString': '', 'display': True}}],
            'yAxes': [{'type': 'linear', 'position': 'left', 'scaleLabel': {'labelString': '', 'display': True}, 'display': True, 'ticks': { 'min': 0,  }}],
         }
    }
}

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
