let timgeo = {
copyArea: function(e1) {
   const isIOS = navigator.userAgent.match(/ipad|ipod|iphone/i);
   if (isIOS) {
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
    if (commands ) {
        labels = api.evalCommandGetLabels(commands);
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


setPointsCoords(api, lines) {
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


};
