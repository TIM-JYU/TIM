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

getObjXML: function(api, name) {
    let names = name.split(",");
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       vals += "\n" + api.getXML(n);
    }
    return vals.trim();
},

getObjValue: function(api, name) {
    let names = name.split(",");
    let vals = "";
    for (let i=0; i < names.length; i++) {
       let n = names[i].trim();
       if ( !n ) continue;
       vals += "\n" + api.getValueString(n);
    }
    return vals.trim();
},

getObjsCommands: function(api, name) {
    let names = name.split(",");
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
    if ( !data ) return;
    if ( data.startsWith("<") ) {
        api.setXML(data);
    } else {
        api.setBase64(data);
    }
},

};
