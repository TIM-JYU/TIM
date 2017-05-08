import {$sanitize} from "./ngimport";

export function initAttributes(clone, $scope) {
"use strict";
    if (!clone[0]) return;
    var markJSON = "xxxJSONxxx";
    var markHex = "xxxHEXJSONxxx";
    var s = clone[0].textContent;
    var chex = s.indexOf(markHex) === 0;
    var cjson = s.indexOf(markJSON) === 0;
    if (!chex && !cjson) {
        return;
    }
    if (cjson) s = s.substring(markJSON.length);
    if (chex) s = Hex2Str(s.substring(markHex.length));
    $scope.attrs = JSON.parse(s);
    $scope.scope = $scope;
}


export function getHeading($scope,attrs,key,defElem) {
"use strict";
	var h = set($scope,attrs,key,"");
	if ( !h ) return "";
	var st = h.split("!!"); // h4 class="h3" width="23"!!Tehtava 1
	var elem = defElem;
	var val = st[0];
	var attributes = "";
	if ( st.length >= 2 ) { elem = st[0]; val = st[1]; }
	var i = elem.indexOf(' ');
	var ea = [elem];
	if ( i >= 0 ) ea = [elem.substring(0,i),elem.substring(i)];
	if ( ea.length > 1 ) { elem = ea[0]; attributes = " " + ea[1] + " "; }
	try { 
	  val = decodeURIComponent(encodeURI(val));
	} catch(err) {}
    var html = "<" + elem + attributes + ">" + val + "</" + elem + ">";
	html = $sanitize(html);
	return html;
}


export function get(jso,keys) {
"use strict";
    if ( !jso ) return undefined;
    var val = jso;
    for (var k in keys ) {
        if ( val === null ) return undefined;
        if ( !keys[k] ) continue;
        val = val[keys[k]];
        if ( val === undefined ) return undefined
    }
    return val;
}


export function setk(scope,sname,attrs,keys,def) {
"use strict";
    scope[sname] = def;
    var val = get(attrs,keys);
    if ( val != undefined ) scope[sname] = val;
    val = get(scope.attrs,keys);
    if ( val != undefined ) scope[sname] = val;
    if ( scope[sname] == "None" ) scope[sname] = "";
    return scope[sname];
}


export function setn(scope,sname,attrs,name,def?) {
"use strict";
    if ( name.indexOf(".") < 0 ) name = "markup."+name;
    var keys = name.split(".");
    return setk(scope,sname,attrs,keys,def);
}


export function set(scope,attrs,name,def?) {
"use strict";
    if ( name.indexOf(".") < 0 ) name = "markup."+name;
    var keys = name.split(".");
    var sname = keys[keys.length-1];
    return setk(scope,sname,attrs,keys,def);
}


export function Hex2Str(s) {
"use strict";
  var result = '';
  for (var i=0; i<s.length; i+=2) {
    var c = String.fromCharCode(parseInt(s[i]+s[i+1],16));
    result += c;
  }
  return result;
}
