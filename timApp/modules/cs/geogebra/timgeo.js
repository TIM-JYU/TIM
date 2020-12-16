let timgeo = {
copyArea: function(e1) {
   if (isIOS()) {
       e1.contentEditable = true;
       e1.readOnly = true;
       let range = document.createRange();
       range.selectNodeContents(e1);
       let sel = window.getSelection();
       if ( sel ) {
          sel.removeAllRanges();
          sel.addRange(range);
       }
       e1.setSelectionRange(0, 999999);
       e1.readOnly = false;
   } else {
      e1.select();
   }
   document.execCommand('copy');
},

getConstructionState: function(api) {
    let n = api.getObjectNumber();
    let state = "Number of objects: " + n;
    for (let i=0; i < n; i++) {
	    let name = api.getObjectName(i);
	    let objType = api.getObjectType(name);
	    let cmd = api.getCommandString(name, false);
	    let strVal = api.getValueString(name);
	    state += "\n" + objType + " " + name + "; " + cmd + "; " + strVal;
    }
    return state;
},


makeArray: function(objnames) {
    if ( Array.isArray(objnames) ) return objnames;
    if ( !objnames ) return [];
    return objnames.split(",");
},

getObjXML: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       vals += "\n" + api.getXML(n);
    }
    return vals.trim();
},

getObjValue: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       vals += "\n" + api.getValueString(n);
    }
    return vals.trim();
},

getObjsCommands: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let cmds = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       cmds += "\n" + timgeo.getObjCommand(api, n);
    }
    return cmds.trim();
},

getObjCommand: function(api, name) {
    if ( name.indexOf(",") >= 0 ) return timgeo.getObjsCommands(api, name);
    let cmd = api.getCommandString(name, false);
    let objType = api.getObjectType(name);
    if ( !cmd )
        cmd = api.getValueString(name);
    if ( objType === "text" )  {
        if ( cmd.indexOf('"') < 0 )
            cmd = '"' + cmd + '"';
    } else
        if ( cmd.indexOf("=") >= 0 ) name = "";
    if ( name !== "" ) name += ": ";
    return name + cmd;
},

getCommands: function(api) {
    let n = api.getObjectNumber();
    let cmds = "";
    for (let i=0; i < n; i++) {
	    let name = api.getObjectName(i);
	    let cmd = timgeo.getObjCommand(api, name);
	    cmds += "\n" + cmd;
    }
    return cmds.trim();
},

setState: function(api, geostate) {
    if ( !geostate ) return;
    let data = geostate['data'];
    if ( data ) {
        if (data.startsWith("<")) {
            api.setXML(data);
        } else {
            api.setBase64(data);
        }
    }
    let commands = geostate['commands'];
    if ( commands ) {
        let labels = api.evalCommandGetLabels(commands);
    }
    let objxml = geostate['objxml'];
    if ( objxml ) {
        api.evalXML(objxml);
    }
},

setLabelsVisible: function (api, objnames, visible)  {
    let names = timgeo.makeArray(objnames);
    for (let i=0; i < names.length; i++) {
        let n = names[i].trim();
        if ( !n ) continue;
        api.setLabelVisible(n, visible);
    }
},

setLabelStyle: function (api, objnames, style)  {
    let names = timgeo.makeArray(objnames);
    for (let i=0; i < names.length; i++) {
        let n = names[i].trim();
        if ( !n ) continue;
        api.setLabelVisible(n, true);
        api.setLabelStyle(n, style);
    }
},

setAllLabelsVisible: function (api, visible)  {
    let objnames = api.getAllObjectNames();
    timgeo.setLabelsVisible(api, objnames, visible);
},


setPointsCoords: function (api, lines) {
    // B = (-0.88, -0.47)
    if ( !lines ) return;
    cmds = lines.split('\n');
    let re = / *([^ =]*) *= *[\(\[]([^,]*), *([^\)\]]*)/;
    for (let i=0; i < cmds.length; i++) {
        let cmd = cmds[i].trim();
        if ( !cmd ) continue;
        let m = re.exec(cmd);
        if ( !m ) continue;
        api.setCoords(m[1], m[2], m[3]);
    }
},


setXmlPropertyLines: function (lines, obj, prop) {
    let name = '<' + prop.substr(1, prop.indexOf(' ') - 1);
    let found = false;
    let endi = 0;
    for (let i = 0; i < lines.length; i++) {
        let line = lines[i];
        if (line.indexOf(name) >= 0) {
            lines[i] += prop;
            found = true;
            continue;
        }
        if (line.indexOf("</element>") >= 0) endi = i;
    }
    if (!found)
        lines.splice(endi, 0, prop);
},

setXmlProperty: function (api, obj, prop) {
//  https://help.geogebra.org/topic/dynamically-changing-size-of-an-input-box-based-on-input-
    let xml = api.getXML(obj);
    let lines = xml.split("\n");
    let props = prop.split("\n");
    for (let i = 0; i < props.length; i++) {
        let pr = props[i].trim();
        if ( pr ) timgeo.setXmlPropertyLines(lines, obj, pr);
    }
    xml = lines.join("\n");
    api.evalXML(xml);
},

setTextValue: function (api, name, value) {
    api.setTextValue(name, value || '');
},

getPureValue: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       let s = ggbApplet.getValueString(n);
       vals += "\n" + s.replace(/[^.,\-0-9]/g, '');
    }
    return vals.trim();
},

getNumberValue: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       let s = ggbApplet.getValue(n);
       vals += "\n" + s;
    }
    return vals.trim();
},

deleteObject: function(api, objnames) {
    let names = timgeo.makeArray(objnames);
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       let s = ggbApplet.deleteObject(n);
    }
},

deleteAllObjects: function(api) {
    timgeo.deleteObject(api, ggbApplet.getAllObjectNames());
},

removeLines: function(s, remove) {
    let re = new RegExp("^.*" + remove + '.*\n?', "gm");
    return s.replace(re, '');
},


};
